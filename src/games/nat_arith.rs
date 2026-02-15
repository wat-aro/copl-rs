pub(crate) trait NatTermLike: PartialEq {
    fn is_zero(&self) -> bool;
    fn succ_inner(&self) -> Option<&Self>;
}

pub(crate) struct PSuccConclusion<'a, T: NatTermLike> {
    pub(crate) n1: &'a T,
    pub(crate) n2: &'a T,
    pub(crate) n: &'a T,
}

pub(crate) struct TSuccConclusion<'a, T: NatTermLike> {
    pub(crate) n1: &'a T,
    pub(crate) n2: &'a T,
    pub(crate) n4: &'a T,
}

pub(crate) fn is_p_zero_conclusion<T: NatTermLike>(left: &T, right: &T, result: &T) -> bool {
    left.is_zero() && right == result
}

pub(crate) fn p_succ_conclusion<'a, T: NatTermLike>(
    left: &'a T,
    right: &'a T,
    result: &'a T,
) -> Option<PSuccConclusion<'a, T>> {
    Some(PSuccConclusion {
        n1: left.succ_inner()?,
        n2: right,
        n: result.succ_inner()?,
    })
}

pub(crate) fn p_succ_premise_matches<T: NatTermLike>(
    conclusion: &PSuccConclusion<'_, T>,
    premise_left: &T,
    premise_right: &T,
    premise_result: &T,
) -> bool {
    premise_left == conclusion.n1
        && premise_right == conclusion.n2
        && premise_result == conclusion.n
}

pub(crate) fn is_t_zero_conclusion<T: NatTermLike>(left: &T, result: &T) -> bool {
    left.is_zero() && result.is_zero()
}

pub(crate) fn t_succ_conclusion<'a, T: NatTermLike>(
    left: &'a T,
    right: &'a T,
    result: &'a T,
) -> Option<TSuccConclusion<'a, T>> {
    Some(TSuccConclusion {
        n1: left.succ_inner()?,
        n2: right,
        n4: result,
    })
}

pub(crate) fn t_succ_premises_match<T: NatTermLike>(
    conclusion: &TSuccConclusion<'_, T>,
    first_left: &T,
    first_right: &T,
    first_n3: &T,
    second_left: &T,
    second_right: &T,
    second_result: &T,
) -> bool {
    first_left == conclusion.n1
        && first_right == conclusion.n2
        && second_left == first_right
        && second_left == conclusion.n2
        && second_right == first_n3
        && second_result == conclusion.n4
}

#[cfg(test)]
mod tests {
    use super::{
        is_p_zero_conclusion, is_t_zero_conclusion, p_succ_conclusion, p_succ_premise_matches,
        t_succ_conclusion, t_succ_premises_match, NatTermLike,
    };

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum Term {
        Z,
        S(Box<Term>),
    }

    impl NatTermLike for Term {
        fn is_zero(&self) -> bool {
            matches!(self, Self::Z)
        }

        fn succ_inner(&self) -> Option<&Self> {
            let Self::S(inner) = self else {
                return None;
            };
            Some(inner.as_ref())
        }
    }

    fn z() -> Term {
        Term::Z
    }

    fn s(inner: Term) -> Term {
        Term::S(Box::new(inner))
    }

    #[test]
    fn validates_p_zero_shape() {
        assert!(is_p_zero_conclusion(&z(), &s(z()), &s(z())));
        assert!(!is_p_zero_conclusion(&s(z()), &s(z()), &s(z())));
    }

    #[test]
    fn validates_p_succ_links() {
        let left = s(s(z()));
        let right = s(z());
        let result = s(s(s(z())));
        let Some(conclusion) = p_succ_conclusion(&left, &right, &result) else {
            panic!("expected p-succ conclusion");
        };

        assert!(p_succ_premise_matches(
            &conclusion,
            &s(z()),
            &s(z()),
            &s(s(z()))
        ));
        assert!(!p_succ_premise_matches(
            &conclusion,
            &z(),
            &s(z()),
            &s(s(z()))
        ));
    }

    #[test]
    fn validates_t_zero_shape() {
        assert!(is_t_zero_conclusion(&z(), &z()));
        assert!(!is_t_zero_conclusion(&s(z()), &z()));
        assert!(!is_t_zero_conclusion(&z(), &s(z())));
    }

    #[test]
    fn validates_t_succ_links() {
        let left = s(s(z()));
        let right = s(z());
        let result = s(s(s(z())));
        let Some(conclusion) = t_succ_conclusion(&left, &right, &result) else {
            panic!("expected t-succ conclusion");
        };

        assert!(t_succ_premises_match(
            &conclusion,
            &s(z()),
            &s(z()),
            &s(s(z())),
            &s(z()),
            &s(s(z())),
            &s(s(s(z())))
        ));
        assert!(!t_succ_premises_match(
            &conclusion,
            &z(),
            &s(z()),
            &s(s(z())),
            &s(z()),
            &s(s(z())),
            &s(s(s(z())))
        ));
    }
}
