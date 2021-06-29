package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.SearchVO;

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
    List<GanttChartVO> pagedQuery(Long projectId, SearchVO searchVO, Long lastIssueId, Integer size);

    /**
     * 查询甘特图列表数据(用户视图)
     *
     * @param projectId
     * @param searchVO
     * @return
     */
//    List<GanttChartTreeVO> listByUser(Long projectId, SearchVO searchVO);
}
