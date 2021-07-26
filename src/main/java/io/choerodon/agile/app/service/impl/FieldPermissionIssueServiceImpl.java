package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.PageFieldViewParamVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;
import io.choerodon.agile.app.service.FieldPermissionIssueService;
import io.choerodon.agile.app.service.FieldPermissionService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.PageFieldService;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2021-07-23
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FieldPermissionIssueServiceImpl implements FieldPermissionIssueService {

    @Autowired
    private IssueService issueService;
    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private FieldPermissionService fieldPermissionService;

    @Override
    public List<PageFieldViewVO> listNoPermissionRequiredFields(Long organizationId,
                                                                Long projectId,
                                                                PageFieldViewParamVO paramDTO,
                                                                Long issueId) {
        Long issueTypeId = paramDTO.getIssueTypeId();
        AssertUtilsForCommonException.notNull(issueTypeId, "error.issue.type.id.null");
        String pageCode = paramDTO.getPageCode();
        List<PageFieldViewVO> result;
        if (StringUtils.isEmpty(pageCode)) {
            //问题类型转换，查创建和编辑页的所有必填字段
            AssertUtilsForCommonException.notNull(issueId, "error.issue.id.null");
            result = issueService.listRequiredFieldByIssueTypeNoFilter(projectId, organizationId, issueId, issueTypeId);
        } else {
            result = pageFieldService.queryPageFieldViewsNoPermissionFilter(organizationId, projectId, paramDTO)
                    .stream()
                    .filter(x -> Boolean.TRUE.equals(x.getRequired()))
                    .collect(Collectors.toList());
        }
        return fieldPermissionService.filterNoPermissionFields(projectId, organizationId, issueTypeId, result);
    }
}
