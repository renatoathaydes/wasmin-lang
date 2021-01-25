pub(crate) fn remove_last_n<T>(vec: &mut Vec<T>, n: usize) {
    let mut len = vec.len();
    let limit = len - n;
    while len > limit {
        len -= 1;
        let _ = vec.remove(len);
    }
}

pub(crate) fn push_all<T: Clone>(source: &Vec<T>, dest: &mut Vec<T>) {
    for element in source {
        dest.push(element.clone());
    }
}

pub(crate) fn get_last<T>(vec: &mut Vec<T>) -> &mut T {
    let len = vec.len();
    vec.get_mut(len - 1).unwrap()
}