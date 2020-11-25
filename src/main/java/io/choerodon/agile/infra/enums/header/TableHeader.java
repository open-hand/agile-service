package io.choerodon.agile.infra.enums.header;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.TableHeaderVO;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author superlee
 * @since 2020-11-25
 */
public class TableHeader {

    private static final Map<String, List<TableHeaderVO>> map;

    static  {
        map = new HashMap<>();
        map.put(TableHeaderCode.GANTT_CHART.code(), GanttChart.headers());
    }

    public static List<TableHeaderVO> get(String key) {
        List<TableHeaderVO> result = map.get(key);
        if (result == null) {
            result = new ArrayList<>();
        }
        return result;
    }

    public static class GanttChart {
        private static final List<TableHeaderVO> TABLE_HEADERS;

        static {
            TABLE_HEADERS = new ArrayList<>();
            TABLE_HEADERS.add(new TableHeaderVO(1, "名称", GanttChartVO.FIELD_SUMMARY));
            TABLE_HEADERS.add(new TableHeaderVO(2, "经办人", GanttChartVO.FIELD_ASSIGNEE));
            TABLE_HEADERS.add(new TableHeaderVO(3, "预计开始时间", GanttChartVO.FIELD_ESTIMATED_START_TIME));
            TABLE_HEADERS.add(new TableHeaderVO(4, "预计结束时间", GanttChartVO.FIELD_ESTIMATED_END_TIME));
        }

        public static List<TableHeaderVO> headers() {
            return TABLE_HEADERS;
        }
    }
}
