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
     * 查询甘特图列表数据
     *
     * @param projectId
     * @param searchVO
     * @return
     */
    List<GanttChartVO> list(Long projectId, SearchVO searchVO);
}
