package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.IssueTypeNoPermissionFields;
import io.choerodon.agile.api.vo.PageFieldViewParamVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;

/**
 * @author superlee
 * @since 2021-07-23
 */
public interface FieldPermissionIssueService {

    /**
     * 查询当前用户在某个问题类型下，没有权限的必填字段
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param paramDTO paramDTO
     * @param issueId issueId
     * @return result
     */
    List<PageFieldViewVO> listNoPermissionRequiredFields(Long organizationId,
                                                         Long projectId,
                                                         PageFieldViewParamVO paramDTO,
                                                         Long issueId);

    /**
     * 查询当前用户在所有问题类型下没有权限的字段
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @return result
     */
    List<IssueTypeNoPermissionFields> listNoPermissionFieldsByIssueType(Long organizationId, Long projectId);
}
