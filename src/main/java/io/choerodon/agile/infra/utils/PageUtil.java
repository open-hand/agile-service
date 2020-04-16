package io.choerodon.agile.infra.utils;

import java.util.*;

import com.github.pagehelper.Page;
import com.github.pagehelper.PageInfo;

import org.springframework.data.domain.Sort;
import tk.mybatis.mapper.util.StringUtil;

/**
 * Created by WangZhe@choerodon.io on 2019-06-13.
 * Email: ettwz@hotmail.com
 */
public class PageUtil {
    public static PageInfo buildPageInfoWithPageInfoList(PageInfo pageInfo, List list) {
        Page page = new Page<>(pageInfo.getPageNum(), pageInfo.getPageSize());
        page.setTotal(pageInfo.getTotal());
        page.addAll(list);

        return page.toPageInfo();
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
                        if(order.getDirection().isAscending()){
                            order = Sort.Order.asc(entry.getValue());
                        }
                        else {
                            order = Sort.Order.desc(entry.getValue());
                        }
                        flag = true;
                    }
                }
                if (!flag) {
                    //驼峰转下划线
                    if(order.getDirection().isAscending()){
                        if (mainTableAlias != null) {
                            order = Sort.Order.by(mainTableAlias + "." + StringUtil.camelhumpToUnderline(order.getProperty()));
                        } else {
                            order = Sort.Order.by(StringUtil.camelhumpToUnderline(order.getProperty()));
                        }
                    } else {
                        if (mainTableAlias != null) {
                            order = Sort.Order.desc(mainTableAlias + "." + StringUtil.camelhumpToUnderline(order.getProperty()));
                        } else {
                            order = Sort.Order.desc(StringUtil.camelhumpToUnderline(order.getProperty()));
                        }
                    }
                }
                orders.add(order);
            }
        }
        return Sort.by(orders);
    }

    public static int getBegin(int page, int size) {
        page = page <= 1 ? 1 : page;
        return (page - 1) * size;
    }

    public static PageInfo emptyPageInfo(int page, int size) {
        Page result = new Page(page, size);
        try {
            return result.toPageInfo();
        } finally {
            result.close();
        }
    }

    public static PageInfo getPageInfo(int page, int size, int total, Collection list) {
        Page result = new Page(page, size);
        try {
            result.setTotal(total);
            result.addAll(list);
            return result.toPageInfo();
        } finally {
            result.close();
        }
    }

}
