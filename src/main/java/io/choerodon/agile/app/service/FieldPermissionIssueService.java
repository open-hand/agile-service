package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.IssueTypeNoPermissionFields;
import io.choerodon.agile.api.vo.PageFieldViewParamVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;

import java.util.List;

/**
 * @author superlee
 * @since 2021-07-23
 */
public interface FieldPermissionIssueService {

    /**
     * 查询当前用户在某个问题类型下，没有权限的必填字段
     *
     * @param organizationId
     * @param projectId
     * @param paramDTO
     * @param issueId
     * @return
     */
    List<PageFieldViewVO> listNoPermissionRequiredFields(Long organizationId,
                                                         Long projectId,
                                                         PageFieldViewParamVO paramDTO,
                                                         Long issueId);

    /**
     * 查询当前用户在所有问题类型下没有权限的字段
     *
     * @param organizationId
     * @param projectId
     * @return
     */
    List<IssueTypeNoPermissionFields> listNoPermissionFieldsByIssueType(Long organizationId, Long projectId);
}
