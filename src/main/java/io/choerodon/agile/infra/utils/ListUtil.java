package io.choerodon.agile.infra.utils;

import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-06-01 17:17
 */
public class ListUtil {

    public static <E> List<E> filterByKey(List<E> list, E e) {
        if (CollectionUtils.isEmpty(list)) {
            return new ArrayList<>();
        }
        return list.stream().filter(v -> !Objects.equals(e, v)).collect(Collectors.toList());
    }
}
