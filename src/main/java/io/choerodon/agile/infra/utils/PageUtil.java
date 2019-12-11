package io.choerodon.agile.infra.utils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.github.pagehelper.Page;
import com.github.pagehelper.PageInfo;

import org.springframework.data.domain.Sort;

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
                if (mainTableAlias != null && !flag) {
                    //驼峰转下划线
                    if(order.getDirection().isAscending()){
                        order = Sort.Order.by(mainTableAlias + "." + tk.mybatis.mapper.util.StringUtil.camelhumpToUnderline(order.getProperty()));
                    }
                    else {
                        order = Sort.Order.desc(mainTableAlias + "." + tk.mybatis.mapper.util.StringUtil.camelhumpToUnderline(order.getProperty()));
                    }
                }
                orders.add(order);
            }
        }
        return Sort.by(orders);
    }

}
