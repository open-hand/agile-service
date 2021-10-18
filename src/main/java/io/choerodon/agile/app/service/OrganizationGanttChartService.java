package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author superlee
 * @since 2021-10-18
 */
public interface OrganizationGanttChartService {

    Page<GanttChartVO> pagedQuery(Long organizationId, SearchVO searchVO, PageRequest pageRequest);
}
