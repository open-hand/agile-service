package io.choerodon.agile.infra.utils;

import java.util.*;

import io.choerodon.core.domain.PageInfo;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.util.StringUtil;

import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.Sort;


/**
 * Created by WangZhe@choerodon.io on 2019-06-13.
 * Email: ettwz@hotmail.com
 */
public class PageUtil {
    public static Page buildPageInfoWithPageInfoList(Page page, List list) {
        Page result = new Page<>();
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
        page = page <= 1 ? 1 : page;
        return (page - 1) * size;
    }

    public static Page emptyPage(int page, int size) {
        PageInfo pageInfo = new PageInfo(page, size);
        return new Page(new ArrayList(), pageInfo, 0);
    }

    public static Page getPageInfo(int page, int size, int total, Collection list) {
        Page result = new Page();
        result.setNumber(page);
        result.setSize(size);
        result.setTotalElements(total);
        result.setContent(Arrays.asList(list.toArray()));
        return result;
    }

    public static void buildPage(Page page, PageRequest pageRequest, List<Long> all){
        boolean queryAll = pageRequest.getPage() < 0 || pageRequest.getSize() == 0;
        page.setSize(queryAll ? all.size() : pageRequest.getSize());
        page.setNumber(pageRequest.getPage());
        page.setTotalElements(all.size());
        page.setTotalPages(queryAll ? (all.isEmpty() ? 0 : 1) : ((int) (Math.ceil(all.size() / (pageRequest.getSize() * 1.0)))));
    }

}
