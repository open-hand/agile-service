package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.WorkHoursLogVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-15 13:35
 */
public interface WorkHoursService {
    /**
     *  查询工时日志
     * @param organizationId
     * @param projectIds
     * @param pageRequest
     * @param workHoursSearchVO
     * @return
     */
    Page<WorkHoursLogVO> pageWorkHoursLogByProjectIds(Long organizationId, List<Long> projectIds, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO);

    /**
     *
     * @param organizationId
     * @param pageRequest
     * @param workHoursSearchVO
     * @return
     */
    Page<WorkHoursLogVO> pageWorkHoursLogByOrgId(Long organizationId, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO);

}
