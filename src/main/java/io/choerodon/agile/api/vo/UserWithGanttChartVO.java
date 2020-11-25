package io.choerodon.agile.api.vo;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-25
 */
public class UserWithGanttChartVO {

    private String name;

    private List<GanttChartVO> ganttChartList;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<GanttChartVO> getGanttChartList() {
        return ganttChartList;
    }

    public void setGanttChartList(List<GanttChartVO> ganttChartList) {
        this.ganttChartList = ganttChartList;
    }
}
