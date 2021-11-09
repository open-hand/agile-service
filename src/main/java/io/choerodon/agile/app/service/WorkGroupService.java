package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.MoveWorkGroupVO;
import io.choerodon.agile.api.vo.WorkGroupTreeVO;
import io.choerodon.agile.api.vo.WorkGroupVO;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-11-08 15:31
 */
public interface WorkGroupService {

    WorkGroupTreeVO queryWorkGroupTree(Long organizationId);

    WorkGroupVO create(Long organizationId, WorkGroupVO workGroupVO);

    WorkGroupVO update(Long organizationId, WorkGroupVO workGroupVO);

    void delete(Long organizationId, Long workGroupId);

    WorkGroupVO queryById(Long organizationId, Long workGroupId);

    Boolean checkName(Long organizationId, Long parentId, String name);

    WorkGroupVO moveWorkGroup(Long organizationId, Long parentId, MoveWorkGroupVO moveWorkGroupVO);

    List<Long> listChildrenWorkGroup(Long organizationId, Long workGroupId);
}
