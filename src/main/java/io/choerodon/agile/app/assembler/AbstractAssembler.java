package io.choerodon.agile.app.assembler;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;

import io.choerodon.core.exception.CommonException;

/**
 * 抽象Assembler转换类,如果需要简单的转换，继承此类即可，要实现自己的，则重写方法
 *
 * @author dinghuang123@gmail.com
 * * @since 2018/9/7
 */
abstract class AbstractAssembler {

    private static final String ERROR_CONVERT = "error.abstractAssembler.convert";

    /**
     * 转换到目标类
     *
     * @param source source
     * @return target
     */
    public <T, V> V toTarget(T source, Class<V> tClass) {
        if (source == null || tClass == null) {
            return null;
        } else {
            try {
                V target = tClass.getConstructor().newInstance();
                BeanUtils.copyProperties(source, target);
                return target;
            } catch (NoSuchMethodException | InstantiationException| InvocationTargetException | IllegalAccessException  e) {
                throw new CommonException(ERROR_CONVERT, e);
            }
        }
    }

    /**
     * List转换到目标类
     *
     * @param source source
     * @return target
     */
    @SuppressWarnings("unchecked")
    public <T extends List, V> List<V> toTargetList(T source, Class<V> tClass) {
        if (source == null) {
            return new ArrayList<>();
        } else {
            List<V> targetList = new ArrayList<>(source.size());
            if (!source.isEmpty()) {
                source.forEach(s -> {
                    V target = toTarget(s, tClass);
                    targetList.add(target);
                });
            }
            return targetList;
        }
    }


}
