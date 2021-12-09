package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.FieldPermissionService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.FieldPermissionDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldPermissionScope;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.FieldPermissionMapper;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.collections.map.MultiKeyMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2021-07-20
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FieldPermissionServiceImpl implements FieldPermissionService {

    private static final String USER = "user";
    private static final String ROLE = "role";

    private static final List<String> IGNORED_FIELDS =
            Arrays.asList(
                    FieldCode.ISSUE_TYPE,
                    FieldCode.SUMMARY,
                    FieldCode.DESCRIPTION,
                    FieldCode.ASSIGNEE,
                    FieldCode.PRIORITY,
                    FieldCode.REMAINING_TIME,
                    FieldCode.STORY_POINTS,
                    FieldCode.CREATOR,
                    FieldCode.CREATION_DATE,
                    FieldCode.UPDATOR,
                    FieldCode.LAST_UPDATE_DATE,
                    FieldCode.TIME_TRACE,
                    FieldCode.STATUS,
                    FieldCode.EPIC,
                    FieldCode.FEATURE,
                    FieldCode.ESTIMATED_START_TIME,
                    FieldCode.ESTIMATED_END_TIME,
                    FieldCode.FIX_VERSION,
                    FieldCode.INFLUENCE_VERSION,
                    FieldCode.PROGRAM_VERSION,
                    FieldCode.SPRINT,
                    FieldCode.ENVIRONMENT,
                    FieldCode.MAIN_RESPONSIBLE,
                    FieldCode.EPIC_NAME,
                    FieldCode.FEATURE_TYPE,
                    FieldCode.TAG,
                    FieldCode.PI,
                    FieldCode.SUB_PROJECT,
                    FieldCode.REPORTER,
                    FieldCode.BELONG_TO_BACKLOG,
                    FieldCode.BACKLOG_TYPE,
                    FieldCode.BACKLOG_CLASSIFICATION,
                    FieldCode.EMAIL,
                    FieldCode.PROGRESS_FEEDBACK,
                    FieldCode.URGENT
            );

    @Autowired
    private FieldPermissionMapper fieldPermissionMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private BaseFeignClient baseFeignClient;

    @Override
    public void create(Long projectId, Long organizationId, FieldPermissionVO fieldPermissionVO) {
        Map<String, Long> filedMap = new HashMap<>();
        validateFieldPermission(fieldPermissionVO, filedMap);
        Set<Long> fieldIds = filterIgnoredFields(filedMap);
        AssertUtilsForCommonException.notEmpty(fieldIds, "error.allowed.fields.null");
        Set<Long> issueTypeIds = fieldPermissionVO.getIssueTypeIds();
        List<PermissionVO> permissions = fieldPermissionVO.getPermissions();
        List<FieldPermissionDTO> fieldPermissionList =
                fieldPermissionMapper.selectByFieldIds(projectId, organizationId, fieldIds, issueTypeIds);
        //key1 fieldId, key2 issueTypeId, key3 permission, value Map<type, List<FieldPermissionDTO>>
        MultiKeyMap fieldPermissionKeyMap = new MultiKeyMap();
        //key1 fieldId, key2 roleMemberId, key3 type, key4 permission, key5 issueTypeId, value FieldPermissionDTO
        MultiKeyMap allFieldKeyMap = new MultiKeyMap();
        Set<Long> fieldPermissionIds = new HashSet<>();
        processMap(fieldPermissionList, fieldPermissionKeyMap, allFieldKeyMap, fieldPermissionIds);

        if (!ObjectUtils.isEmpty(permissions)) {
            addOrDeleteFieldPermission(projectId, organizationId, fieldIds, permissions, fieldPermissionKeyMap, allFieldKeyMap, issueTypeIds);
        } else {
            //删除字段权限
            if (!fieldPermissionIds.isEmpty()) {
                fieldPermissionMapper.deleteByIds(fieldPermissionIds);
            }
        }
    }

    @Override
    public void batchCreate(Long projectId, Long organizationId, FieldPermissionVO fieldPermissionVO) {
        create(projectId, organizationId, fieldPermissionVO);
    }

    @Override
    public List<PermissionVO> queryByFieldId(Long projectId,
                                             Long organizationId,
                                             Long fieldId,
                                             Long issueTypeId) {
        List<PermissionVO> result = new ArrayList<>();
        Map<String, Map<String, Set<Long>>> typeRoleMemberMap = new HashMap<>();
        List<FieldPermissionDTO> fieldPermissionList =
                fieldPermissionMapper.selectByFieldIds(projectId,
                        organizationId, new HashSet<>(Arrays.asList(fieldId)), new HashSet<>(Arrays.asList(issueTypeId)));
        fieldPermissionList.forEach(x -> {
            String permission = x.getPermission();
            Map<String, Set<Long>> roleMemberMap =
                    typeRoleMemberMap.computeIfAbsent(permission, y -> new HashMap<>());
            String type = x.getType();
            Set<Long> roleMemberIds = roleMemberMap.computeIfAbsent(type, y -> new HashSet<>());
            roleMemberIds.add(x.getRoleMemberId());
        });
        typeRoleMemberMap.forEach((permission, map) -> {
            PermissionVO vo = new PermissionVO();
            vo.setScope(permission);
            Set<Long> userIds = map.computeIfAbsent(USER, x -> new HashSet<>());
            vo.setUserIds(userIds);
            Set<Long> roleIds = map.computeIfAbsent(ROLE, x -> new HashSet<>());
            vo.setRoleIds(roleIds);
            result.add(vo);
        });
        return result;
    }

    @Override
    public List<String> ignoredFields() {
        return IGNORED_FIELDS;
    }

    @Override
    public void setFieldPermissionList(Long issueTypeId,
                                       Long projectId,
                                       Long organizationId,
                                       List<PageTemplateFieldVO> pageTemplateFieldList) {
        List<PageTemplateFieldVO> allowedEditPermissionFields = new ArrayList<>();
        Set<Long> fieldIds = new HashSet<>();
        filterAllowedEditPermissionList(pageTemplateFieldList, allowedEditPermissionFields, fieldIds);
        if (!fieldIds.isEmpty()) {
            List<FieldPermissionDTO> fieldPermissionList =
                    fieldPermissionMapper.selectByFieldIds(projectId, organizationId, fieldIds, new HashSet<>(Arrays.asList(issueTypeId)));
            if (!fieldPermissionList.isEmpty()) {
                Set<Long> userIds = new HashSet<>();
                Set<Long> roleIds = new HashSet<>();
                //key1 fieldId, key2 permission, value Map<String, Set<Long>>: key type, value role/user ids
                Map<Long, Map<String, Map<String, Set<Long>>>> multiKeyMap = new HashMap<>();
                processMapForPermissionDescription(fieldPermissionList, userIds, roleIds, multiKeyMap);
                Map<Long, UserMessageDTO> userMap = new HashMap<>();
                Map<Long, RoleVO> roleMap = new HashMap<>();
                if (!userIds.isEmpty()) {
                    userMap.putAll(userService.queryUsersMap(new ArrayList<>(userIds), true));
                }
                if (!roleIds.isEmpty()) {
                    roleMap.putAll(
                            baseFeignClient.listRolesByIds(organizationId, new ArrayList<>(roleIds))
                                    .getBody()
                                    .stream()
                                    .collect(Collectors.toMap(RoleVO::getId, Function.identity())));

                }
                Map<Long, List<PermissionVO>> permissionMap = processPermissionGroupByField(multiKeyMap, userMap, roleMap);
                setPermissionList(allowedEditPermissionFields, permissionMap);
            }
        }
    }

    @Override
    public List<PageFieldViewVO> filterPageFieldViewVO(Long projectId,
                                                       Long organizationId,
                                                       Long issueTypeId,
                                                       List<PageFieldViewVO> pageFieldViews) {
        Set<Long> issueTypeIds = new HashSet<>(Arrays.asList(issueTypeId));
        boolean isPermissionsConfigured = fieldPermissionMapper.isPermissionsConfigured(projectId, organizationId, issueTypeIds);
        CustomUserDetails userDetails = DetailsHelper.getUserDetails();
        boolean isAdmin = Boolean.TRUE.equals(userDetails.getAdmin());
        if (!isPermissionsConfigured || isAdmin) {
            return pageFieldViews;
        }
        List<PageFieldViewVO> result = new ArrayList<>();
        Set<Long> hasPermissionFieldIds = new HashSet<>();
        Set<Long> fieldIds = new HashSet<>();
        pageFieldViews.forEach(x -> {
            if (IGNORED_FIELDS.contains(x.getFieldCode())) {
                hasPermissionFieldIds.add(x.getFieldId());
            } else {
                fieldIds.add(x.getFieldId());
            }
        });
        hasPermissionFieldIds.addAll(filterHavingPermissionFieldIds(projectId, organizationId, issueTypeIds, fieldIds));
        pageFieldViews.forEach(field -> {
            if (hasPermissionFieldIds.contains(field.getFieldId())) {
                result.add(field);
            }
        });
        return result;
    }

    private Set<Long> filterHavingPermissionFieldIds(Long projectId,
                                                     Long organizationId,
                                                     Set<Long> issueTypeIds,
                                                     Set<Long> fieldIds) {
        Set<Long> hasPermissionFieldIds = new HashSet<>();
        if (!fieldIds.isEmpty()) {
            CustomUserDetails userDetails = DetailsHelper.getUserDetails();
            Long userId = userDetails.getUserId();
            List<RoleVO> roles = baseFeignClient.getUserWithProjLevelRolesByUserId(projectId, userId).getBody();
            Set<Long> userIds = new HashSet<>(Arrays.asList(userId));
            Set<Long> roleIds = new HashSet<>();
            if (!ObjectUtils.isEmpty(roles)) {
                roleIds.addAll(roles.stream().map(RoleVO::getId).collect(Collectors.toSet()));
            }
            PermissionVO permissionVO = new PermissionVO();
            permissionVO.setScope(FieldPermissionScope.READ.value());
            permissionVO.setUserIds(userIds);
            permissionVO.setRoleIds(roleIds);
            hasPermissionFieldIds.addAll(
                    fieldPermissionMapper.filterHasPermissionFields(projectId, organizationId, issueTypeIds, permissionVO, fieldIds));
        }
        return hasPermissionFieldIds;
    }

    @Override
    public List<PageFieldViewVO> filterNoPermissionFields(Long projectId,
                                                          Long organizationId,
                                                          Long issueTypeId,
                                                          List<PageFieldViewVO> pageFieldViews) {
        Set<Long> issueTypeIds = new HashSet<>(Arrays.asList(issueTypeId));
        boolean isPermissionsConfigured = fieldPermissionMapper.isPermissionsConfigured(projectId, organizationId, issueTypeIds);
        CustomUserDetails userDetails = DetailsHelper.getUserDetails();
        boolean isAdmin = Boolean.TRUE.equals(userDetails.getAdmin());
        if (!isPermissionsConfigured || isAdmin) {
            return new ArrayList<>();
        }
        Set<Long> fieldIds = new HashSet<>();
        List<PageFieldViewVO> allowedEditPermissionList = new ArrayList<>();
        pageFieldViews.forEach(x -> {
            if (!IGNORED_FIELDS.contains(x.getFieldCode())) {
                allowedEditPermissionList.add(x);
                fieldIds.add(x.getFieldId());
            }
        });
        Set<Long> hasPermissionFieldIds =
                filterHavingPermissionFieldIds(projectId, organizationId, issueTypeIds, fieldIds);
        List<PageFieldViewVO> result = new ArrayList<>();
        allowedEditPermissionList.forEach(x -> {
            if (!hasPermissionFieldIds.contains(x.getFieldId())) {
                result.add(x);
            }
        });
        return result;
    }

    private void setPermissionList(List<PageTemplateFieldVO> allowedEditPermissionFields, Map<Long, List<PermissionVO>> permissionMap) {
        if (!permissionMap.isEmpty()) {
            allowedEditPermissionFields.forEach(x -> {
                Long fieldId = x.getFieldId();
                List<PermissionVO> permissionList = permissionMap.get(fieldId);
                if (!ObjectUtils.isEmpty(permissionList)) {
                    x.setPermissionList(permissionList);
                }
            });
        }
    }

    private Map<Long, List<PermissionVO>> processPermissionGroupByField(Map<Long, Map<String, Map<String, Set<Long>>>> multiKeyMap,
                                                                        Map<Long, UserMessageDTO> userMap,
                                                                        Map<Long, RoleVO> roleMap) {
        Map<Long, List<PermissionVO>> resultMap = new HashMap<>();
        multiKeyMap.forEach((fieldId, map) -> {
            List<PermissionVO> permissionList = resultMap.computeIfAbsent(fieldId, x -> new ArrayList<>());
            map.forEach((permission, map1) -> {
                PermissionVO vo = new PermissionVO();
                permissionList.add(vo);
                vo.setScope(permission);
                Set<Long> userIds = map1.computeIfAbsent(USER, x -> new HashSet<>());
                Set<Long> roleIds = map1.computeIfAbsent(ROLE, x -> new HashSet<>());
                List<Long> userIdList = new ArrayList<>(userIds);
                List<Long> roleIdList = new ArrayList<>(roleIds);
                userIdList.sort(Long::compareTo);
                roleIdList.sort(Long::compareTo);
                List<UserMessageDTO> userList = new ArrayList<>();
                List<RoleVO> roleList = new ArrayList<>();
                vo.setUserList(userList);
                vo.setRoleList(roleList);
                userIdList.forEach(x -> {
                    UserMessageDTO user = userMap.get(x);
                    if (user != null) {
                        userList.add(user);
                    }
                });
                roleIdList.forEach(x -> {
                    RoleVO role = roleMap.get(x);
                    if (role != null) {
                        roleList.add(role);
                    }
                });
            });
        });
        return resultMap;
    }

    private void processMapForPermissionDescription(List<FieldPermissionDTO> fieldPermissionList,
                                                    Set<Long> userIds,
                                                    Set<Long> roleIds,
                                                    Map<Long, Map<String, Map<String, Set<Long>>>> multiKeyMap) {
        fieldPermissionList.forEach(x -> {
            String type = x.getType();
            Long roleMemberId = x.getRoleMemberId();
            if (USER.equals(type)) {
                userIds.add(roleMemberId);
            }
            if (ROLE.equals(type)) {
                roleIds.add(roleMemberId);
            }
            Long fieldId = x.getFieldId();
            String permission = x.getPermission();
            Map<String, Map<String, Set<Long>>> map1 = multiKeyMap.computeIfAbsent(fieldId, y -> new HashMap<>());
            Map<String, Set<Long>> map2 = map1.computeIfAbsent(permission, y -> new HashMap<>());
            Set<Long> roleMemberIds = map2.computeIfAbsent(type, y -> new HashSet<>());
            roleMemberIds.add(roleMemberId);
        });
    }

    private void filterAllowedEditPermissionList(List<PageTemplateFieldVO> pageTemplateFieldList,
                                                 List<PageTemplateFieldVO> allowedEditPermissionFields,
                                                 Set<Long> fieldIds) {
        pageTemplateFieldList.forEach(x -> {
            String fieldCode = x.getFieldCode();
            boolean allowedEditPermission = !IGNORED_FIELDS.contains(fieldCode);
            x.setAllowedEditPermission(allowedEditPermission);
            if (allowedEditPermission) {
                allowedEditPermissionFields.add(x);
                fieldIds.add(x.getFieldId());
            }
        });
    }

    private Set<Long> filterIgnoredFields(Map<String, Long> filedMap) {
        Set<Long> result = new HashSet<>();
        filedMap.forEach((k, v) -> {
            if (!IGNORED_FIELDS.contains(k)) {
                result.add(v);
            }
        });
        return result;
    }

    private void addOrDeleteFieldPermission(Long projectId,
                                            Long organizationId,
                                            Set<Long> fieldIds,
                                            List<PermissionVO> permissions,
                                            MultiKeyMap fieldPermissionKeyMap,
                                            MultiKeyMap allFieldKeyMap,
                                            Set<Long> issueTypeIds) {
        List<FieldPermissionDTO> addList = new ArrayList<>();
        List<FieldPermissionDTO> deleteList = new ArrayList<>();
        Map<String, PermissionVO> permissionMap =
                permissions.stream().collect(Collectors.toMap(PermissionVO::getScope, Function.identity()));
        fieldIds.forEach(fieldId -> {
            for (FieldPermissionScope fieldPermissionScope : FieldPermissionScope.values()) {
                String scope = fieldPermissionScope.value();
                PermissionVO permission = permissionMap.get(scope);
                Set<Long> userIds;
                Set<Long> roleIds;
                if (ObjectUtils.isEmpty(permission)) {
                    userIds = new HashSet<>();
                    roleIds = new HashSet<>();
                } else {
                    userIds = getUserIds(permission);
                    roleIds = getRoleIds(permission);
                }
                issueTypeIds.forEach(issueTypeId -> {
                    Map<String, List<FieldPermissionDTO>> map =
                            (Map<String, List<FieldPermissionDTO>>) fieldPermissionKeyMap.get(fieldId, issueTypeId, scope);
                    if (ObjectUtils.isEmpty(map)) {
                        addList.addAll(buildFieldPermissionList(fieldId, userIds, roleIds, projectId, organizationId, scope, issueTypeId));
                    } else {
                        processAddAndDeleteList(projectId, organizationId, addList, deleteList, scope, userIds, fieldId, issueTypeId, map, USER);
                        processAddAndDeleteList(projectId, organizationId, addList, deleteList, scope, roleIds, fieldId, issueTypeId, map, ROLE);
                    }
                });
            }
        });
        //添加的权限如果只有写权限，则加上读权限
        addReadIfNotExisted(addList, allFieldKeyMap, projectId, organizationId);
        //删除读权限时，如果有写权限，一块删除
        deleteWriteIfReadExisted(deleteList, allFieldKeyMap);
        addList.forEach(x -> {
            if (fieldPermissionMapper.select(x).isEmpty()) {
                fieldPermissionMapper.insert(x);
            }
        });
        Set<Long> deleteIds = deleteList.stream().map(FieldPermissionDTO::getId).collect(Collectors.toSet());
        if (!deleteIds.isEmpty()) {
            fieldPermissionMapper.deleteByIds(deleteIds);
        }
    }

    private void deleteWriteIfReadExisted(List<FieldPermissionDTO> deleteList,
                                          MultiKeyMap allFieldKeyMap) {
        List<FieldPermissionDTO> list = new ArrayList<>();
        deleteList.forEach(x -> {
            String permission = x.getPermission();
            if (FieldPermissionScope.isRead(permission)) {
                Long fieldId = x.getFieldId();
                Long roleMemberId = x.getRoleMemberId();
                String type = x.getType();
                Long issueTypeId = x.getIssueTypeId();
                FieldPermissionDTO dto =
                        (FieldPermissionDTO) allFieldKeyMap.get(fieldId, roleMemberId, type, FieldPermissionScope.WRITE.value(), issueTypeId);
                if (dto != null) {
                    list.add(dto);
                }
            }
        });
        deleteList.addAll(list);
    }

    private void processMap(List<FieldPermissionDTO> fieldPermissionList,
                            MultiKeyMap fieldPermissionKeyMap,
                            MultiKeyMap allFieldKeyMap,
                            Set<Long> fieldPermissionIds) {
        fieldPermissionList.forEach(x -> {
            Long fieldId = x.getFieldId();
            String permission = x.getPermission();
            Long issueTypeId = x.getIssueTypeId();
            Map<String, List<FieldPermissionDTO>> map =
                    (Map<String, List<FieldPermissionDTO>>) fieldPermissionKeyMap.get(fieldId, issueTypeId, permission);
            if (map == null) {
                map = new HashMap<>();
                fieldPermissionKeyMap.put(fieldId, issueTypeId, permission, map);
            }
            String type = x.getType();
            List<FieldPermissionDTO> list = map.computeIfAbsent(type, y -> new ArrayList<>());
            list.add(x);
            fieldPermissionIds.add(x.getId());
            allFieldKeyMap.put(fieldId, x.getRoleMemberId(), type, permission, issueTypeId, x);
        });
    }

    private void addReadIfNotExisted(List<FieldPermissionDTO> addList,
                                     MultiKeyMap allFieldKeyMap,
                                     Long projectId,
                                     Long organizationId) {
        List<FieldPermissionDTO> list = new ArrayList<>();
        addList.forEach(x -> {
            String permission = x.getPermission();
            if (FieldPermissionScope.isWrite(permission)) {
                Long fieldId = x.getFieldId();
                Long roleMemberId = x.getRoleMemberId();
                String type = x.getType();
                Long issueTypeId = x.getIssueTypeId();
                Object obj =
                        allFieldKeyMap.get(fieldId, roleMemberId, type, FieldPermissionScope.READ.value(), issueTypeId);
                if (obj == null) {
                    list.add(new FieldPermissionDTO(null, fieldId, issueTypeId, roleMemberId, type, FieldPermissionScope.READ.value(), projectId, organizationId));
                }
            }
        });
        addList.addAll(list);
    }

    private void processAddAndDeleteList(Long projectId,
                                         Long organizationId,
                                         List<FieldPermissionDTO> addList,
                                         List<FieldPermissionDTO> deleteList,
                                         String scope,
                                         Set<Long> roleMemberIds,
                                         Long fieldId,
                                         Long issueTypeId,
                                         Map<String, List<FieldPermissionDTO>> map,
                                         String type) {
        List<FieldPermissionDTO> fieldPermissions = map.computeIfAbsent(type, y -> new ArrayList<>());
        Set<Long> existedRoleMemberIds =
                fieldPermissions.stream().map(FieldPermissionDTO::getRoleMemberId).collect(Collectors.toSet());
        Set<Long> intersection =
                existedRoleMemberIds.stream().filter(y -> roleMemberIds.contains(y)).collect(Collectors.toSet());
        roleMemberIds.forEach(y -> {
            if (!intersection.contains(y)) {
                addList.add(new FieldPermissionDTO(null, fieldId, issueTypeId, y, type, scope, projectId, organizationId));
            }
        });
        fieldPermissions.forEach(y -> {
            Long roleMemberId = y.getRoleMemberId();
            if (!intersection.contains(roleMemberId)) {
                deleteList.add(y);
            }
        });
    }

    private Set<Long> getRoleIds(PermissionVO permissionVO) {
        Set<Long> roleIds = permissionVO.getRoleIds();
        if (roleIds == null) {
            roleIds = new HashSet<>();
        }
        return roleIds;
    }

    private Set<Long> getUserIds(PermissionVO permissionVO) {
        Set<Long> userIds = permissionVO.getUserIds();
        if (userIds == null) {
            userIds = new HashSet<>();
        }
        return userIds;
    }

    private List<FieldPermissionDTO> buildFieldPermissionList(Long fieldId,
                                                              Set<Long> userIds,
                                                              Set<Long> roleIds,
                                                              Long projectId,
                                                              Long organizationId,
                                                              String permission,
                                                              Long issueTypeId) {
        List<FieldPermissionDTO> result = new ArrayList<>();
        if (!ObjectUtils.isEmpty(userIds)) {
            userIds.forEach(x ->
                    result.add(new FieldPermissionDTO(null, fieldId, issueTypeId, x, USER, permission, projectId, organizationId)));
        }
        if (!ObjectUtils.isEmpty(roleIds)) {
            roleIds.forEach(x ->
                    result.add(new FieldPermissionDTO(null, fieldId, issueTypeId, x, ROLE, permission, projectId, organizationId)));
        }
        return result;
    }

    private void validateFieldPermission(FieldPermissionVO fieldPermission,
                                         Map<String, Long> filedMap) {
        AssertUtilsForCommonException.notNull(fieldPermission, "error.field.permission.null");
        List<ObjectSchemeFieldVO> fields = fieldPermission.getFields();
        fields.forEach(x -> {
            Long id = x.getId();
            String code = x.getCode();
            AssertUtilsForCommonException.notNull(id, "error.field.id.null");
            AssertUtilsForCommonException.notEmpty(code, "error.field.code.empty");
            filedMap.put(code, id);
        });
        List<PermissionVO> permissions = fieldPermission.getPermissions();
        if (!ObjectUtils.isEmpty(permissions)) {
            permissions.forEach(x -> {
                String scope = x.getScope();
                if (!FieldPermissionScope.contains(scope)) {
                    throw new CommonException("error.illegal.scope." + scope);
                }
            });
        }
    }
}
