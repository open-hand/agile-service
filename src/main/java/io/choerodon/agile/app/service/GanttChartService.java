package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.UserWithGanttChartVO;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-24
 */
public interface GanttChartService {

    /**
     * 查询甘特图列表数据(任务视图)
     *
     * @param projectId
     * @param searchVO
     * @return
     */
    List<GanttChartVO> listByTask(Long projectId, SearchVO searchVO);

    /**
     * 查询甘特图列表数据(用户视图)
     *
     * @param projectId
     * @param searchVO
     * @return
     */
    List<UserWithGanttChartVO> listByUser(Long projectId, SearchVO searchVO);
}
