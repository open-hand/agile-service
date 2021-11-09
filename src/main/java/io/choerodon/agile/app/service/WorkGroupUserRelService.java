package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.WorkGroupUserRelParamVO;
import io.choerodon.agile.api.vo.WorkGroupUserRelVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2021-11-08 20:39
 */
public interface WorkGroupUserRelService {

    void batchInsertRel(Long organizationId, WorkGroupUserRelParamVO workGroupUserRelParamVO);

    void batchDeleteRel(Long organizationId, WorkGroupUserRelParamVO workGroupUserRelParamVO);

    Page<WorkGroupUserRelVO> pageByQuery(Long organizationId, PageRequest pageRequest, WorkGroupUserRelParamVO workGroupUserRelParamVO);

    Page<WorkGroupUserRelVO> pageUnAssignee(Long organizationId, PageRequest pageRequest,  WorkGroupUserRelParamVO workGroupUserRelParamVO);

    Map<Long, Set<Long>> getWorkGroupMap(Long organizationId);
}
