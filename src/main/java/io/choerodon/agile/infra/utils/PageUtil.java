package io.choerodon.agile.infra.utils;

import java.util.*;

import io.choerodon.core.domain.Page;
import io.choerodon.core.domain.PageInfo;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.mybatis.util.StringUtil;


/**
 * Created by WangZhe@choerodon.io on 2019-06-13.
 * Email: ettwz@hotmail.com
 */
public class PageUtil {

    protected PageUtil() {
        throw new UnsupportedOperationException();
    }

    public static <T,R> Page<R> buildPageInfoWithPageInfoList(Page<T> page, List<R> list) {
        Page<R> result = new Page<>();
        result.setNumber(page.getNumber());
        result.setSize(page.getSize());
        result.setTotalElements(page.getTotalElements());
        result.setTotalPages(page.getTotalPages());
        result.setContent(list);
        return result;
    }

    public static Sort sortResetOrder(Sort sort, String mainTableAlias, Map<String, String> map) {
        List<Sort.Order> orders = new ArrayList<>();
        if (sort != null) {
            Iterator<Sort.Order> iterator = sort.iterator();
            while (iterator.hasNext()) {
                boolean flag = false;
                Sort.Order order = iterator.next();
                for (Map.Entry<String, String> entry : map.entrySet()) {
                    if (entry.getKey().equals(order.getProperty())) {
                        order.setProperty(entry.getValue());
                        flag = true;
                    }
                }
                if (!flag) {
                    //驼峰转下划线
                    if (mainTableAlias != null) {
                        order.setProperty(mainTableAlias + "." + StringUtil.camelhumpToUnderline(order.getProperty()));
                    } else {
                        order.setProperty(StringUtil.camelhumpToUnderline(order.getProperty()));
                    }
                }
                orders.add(order);
            }
        }
        return new Sort(orders);
    }

    public static int getBegin(int page, int size) {
        page++;
        page = Math.max(page, 1);
        return (page - 1) * size;
    }

    public static <T> Page<T> emptyPage(int page, int size) {
        PageInfo pageInfo = new PageInfo(page, size);
        return new Page<>(new ArrayList<>(), pageInfo, 0);
    }

    public static <T> Page<T> getPageInfo(int page, int size, int total, Collection<T> list) {
        Page<T> result = new Page<>();
        result.setNumber(page);
        result.setSize(size);
        result.setTotalElements(total);
        result.setContent(list == null ? Collections.emptyList() : new ArrayList<>(list));
        return result;
    }

    public static <T> void buildPage(Page<T> page, PageRequest pageRequest, List<Long> all){
        boolean queryAll = pageRequest.getPage() < 0 || pageRequest.getSize() == 0;
        page.setSize(queryAll ? all.size() : pageRequest.getSize());
        page.setNumber(pageRequest.getPage());
        page.setTotalElements(all.size());
        page.setTotalPages(queryAll ? (all.isEmpty() ? 0 : 1) : ((int) (Math.ceil(all.size() / (pageRequest.getSize() * 1.0)))));
    }

}
