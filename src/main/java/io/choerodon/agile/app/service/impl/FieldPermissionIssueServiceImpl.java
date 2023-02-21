package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.FieldPermissionDTO;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.mapper.FieldPermissionMapper;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;

/**
 * @author superlee
 * @since 2021-07-23
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FieldPermissionIssueServiceImpl implements FieldPermissionIssueService {

    private static final String USER = "user";
    private static final String ROLE = "role";

    @Autowired
    private IssueService issueService;
    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private FieldPermissionService fieldPermissionService;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private FieldPermissionMapper fieldPermissionMapper;
    @Autowired
    private RemoteIamOperator remoteIamOperator;

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

    @Override
    public List<IssueTypeNoPermissionFields> listNoPermissionFieldsByIssueType(Long organizationId, Long projectId) {
        List<IssueTypeNoPermissionFields> result = new ArrayList<>();
        Map<Long, IssueTypeNoPermissionFields> resultMap = new HashMap<>();
        // 查询当前项目下的问题类型
        List<IssueTypeWithStateMachineIdVO> issueTypes = projectConfigService.queryIssueTypesWithStateMachineIdByProjectId(projectId, null, null, false);
        Set<Long> issueTypeIds = new HashSet<>();
        issueTypes.forEach(issueType -> {
            issueTypeIds.add(issueType.getId());
            IssueTypeNoPermissionFields issueTypeNoPermissionFields = new IssueTypeNoPermissionFields();
            issueTypeNoPermissionFields.setIssueTypeId(issueType.getId());
            issueTypeNoPermissionFields.setIssueTypeName(issueType.getName());
            issueTypeNoPermissionFields.setTypeCode(issueType.getTypeCode());
            issueTypeNoPermissionFields.setNoPermissionFieldCodes(new ArrayList<>());
            result.add(issueTypeNoPermissionFields);
            resultMap.put(issueType.getId(), issueTypeNoPermissionFields);
        });
        // 查询配置权限的字段
        List<FieldPermissionDTO> fieldPermissionList = fieldPermissionMapper.listPermissionsConfigured(projectId, organizationId, issueTypeIds);
        CustomUserDetails userDetails = DetailsHelper.getUserDetails();
        boolean isAdmin = Boolean.TRUE.equals(userDetails.getAdmin());
        if (ObjectUtils.isEmpty(fieldPermissionList) || isAdmin) {
            return result;
        }
        Long userId = userDetails.getUserId();
        List<RoleVO> roles = Optional.ofNullable(remoteIamOperator.getUserWithProjLevelRolesByUserId(projectId, userId)).orElse(new ArrayList<>());
        Set<Long> userRoleIds = new HashSet<>();
        if (!ObjectUtils.isEmpty(roles)) {
            userRoleIds.addAll(roles.stream().map(RoleVO::getId).collect(Collectors.toSet()));
        }
        Map<Long, List<FieldPermissionDTO>> issueTypeFieldPermissionMap = fieldPermissionList.stream().collect(Collectors.groupingBy(FieldPermissionDTO::getIssueTypeId));
        issueTypeFieldPermissionMap.keySet().forEach(issueTypeId -> {

            List<FieldPermissionDTO> issueTypeFieldPermissionDTOS = issueTypeFieldPermissionMap.get(issueTypeId);
            Map<String, List<FieldPermissionDTO>> fieldPermissionMap = issueTypeFieldPermissionDTOS.stream().collect(Collectors.groupingBy(FieldPermissionDTO::getFieldCode));

            IssueTypeNoPermissionFields issueTypeNoPermissionFields = resultMap.get(issueTypeId);
            List<String> noPermissionFieldCodes = issueTypeNoPermissionFields.getNoPermissionFieldCodes();
            // 当前问题类型下没有权限的字段
            fieldPermissionMap.keySet().forEach(fieldCode -> {
                boolean hasPermission = false;
                List<FieldPermissionDTO> fieldPermissionDTOS = fieldPermissionMap.get(fieldCode);
                List<Long> roleIds = fieldPermissionDTOS.stream()
                        .filter(v -> ROLE.equals(v.getType()))
                        .map(FieldPermissionDTO::getRoleMemberId)
                        .collect(Collectors.toList());
                List<Long> userIds = fieldPermissionDTOS.stream()
                        .filter(v -> USER.equals(v.getType()))
                        .map(FieldPermissionDTO::getRoleMemberId)
                        .collect(Collectors.toList());
                if (!ObjectUtils.isEmpty(userIds) && userIds.contains(userId))  {
                    hasPermission = true;
                }
                long roleCount = userRoleIds.stream().filter(roleIds::contains).count();
                if (!ObjectUtils.isEmpty(roleIds) && roleCount > 0) {
                    hasPermission = true;
                }
                if (Boolean.FALSE.equals(hasPermission)) {
                    noPermissionFieldCodes.add(fieldCode);
                }
            });
        });
        return result;
    }
}
