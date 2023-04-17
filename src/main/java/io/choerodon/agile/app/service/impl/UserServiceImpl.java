package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.RoleAssignmentSearchVO;
import io.choerodon.agile.api.vo.RoleVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;


/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/28
 */
@Component
public class UserServiceImpl implements UserService {

    private final RemoteIamOperator remoteIamOperator;

    @Autowired
    public UserServiceImpl(RemoteIamOperator remoteIamOperator) {
        this.remoteIamOperator = remoteIamOperator;
    }

    @Override
    public UserDTO queryUserNameByOption(Long userId, Boolean withId) {
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        if (userId == null || userId == 0) {
            return new UserDTO();
        } else {
            UserDTO userDTO = remoteIamOperator.query(customUserDetails.getOrganizationId(), userId);
            if (withId) {
                userDTO.setRealName(userDTO.getLoginName() + userDTO.getRealName());
            }
            return userDTO;
        }
    }

    @Override
    public Map<Long, UserMessageDTO> queryUsersMap(List<Long> assigneeIdList, Boolean withLoginName) {
        if (CollectionUtils.isEmpty(assigneeIdList)) {
            return new HashMap<>();
        }
        Map<Long, UserMessageDTO> userMessageMap = new HashMap<>(assigneeIdList.size());
        Long[] assigneeIds = new Long[assigneeIdList.size()];
        assigneeIdList.toArray(assigneeIds);
        List<UserDTO> userDTOS = remoteIamOperator.listUsersByIds(assigneeIds, false);
        if (withLoginName) {
            for (UserDTO userDTO : userDTOS) {
                String ldapName = userDTO.getRealName() + "（" + userDTO.getLoginName() + "）";
                String noLdapName = userDTO.getRealName() + "（" + userDTO.getEmail() + "）";
                UserMessageDTO userMessageDTO = new UserMessageDTO(
                        userDTO.getLdap() ? ldapName : noLdapName,
                        userDTO.getLoginName(),
                        userDTO.getRealName(),
                        userDTO.getImageUrl(),
                        userDTO.getEmail(),
                        userDTO.getLdap(),
                        userDTO.getId()
                );
                userMessageDTO.setAttribute14(userDTO.getAttribute14());
                userMessageDTO.setAttribute15(userDTO.getAttribute15());
                userMessageMap.put(userDTO.getId(), userMessageDTO);
            }
        } else {
            for (UserDTO userDTO : userDTOS) {
                UserMessageDTO userMessageDTO = new UserMessageDTO(
                        userDTO.getRealName(),
                        userDTO.getLoginName(),
                        userDTO.getRealName(),
                        userDTO.getImageUrl(),
                        userDTO.getEmail(),
                        userDTO.getLdap(),
                        userDTO.getId()
                );
                userMessageDTO.setAttribute14(userDTO.getAttribute14());
                userMessageDTO.setAttribute15(userDTO.getAttribute15());
                userMessageMap.put(userDTO.getId(), userMessageDTO);
            }
        }
        return userMessageMap;
    }

    @Override
    public List<UserMessageDTO> queryUsers(List<Long> assigneeIdList, Boolean withLoginName) {
        if (assigneeIdList == null) {
            return new ArrayList<>();
        }
        List<UserMessageDTO> userMessage = new ArrayList<>(assigneeIdList.size());
        if (!assigneeIdList.isEmpty()) {
            Long[] assigneeIds = new Long[assigneeIdList.size()];
            assigneeIdList.toArray(assigneeIds);
            List<UserDTO> userDTOS = remoteIamOperator.listUsersByIds(assigneeIds, false);
            if (withLoginName) {
                userDTOS.forEach(userDO -> {
                    String ldapName = userDO.getRealName() + "（" + userDO.getLoginName() + "）";
                    String noLdapName = userDO.getRealName() + "（" + userDO.getEmail() + "）";
                    UserMessageDTO userMessageDTO = new UserMessageDTO(userDO.getLdap() ? ldapName : noLdapName,
                            userDO.getLoginName(),
                            userDO.getRealName(),
                            userDO.getImageUrl(),
                            userDO.getEmail(),
                            userDO.getLdap(),
                            userDO.getId());
                    userMessageDTO.setCreationDate(userDO.getCreationDate());
                    userMessage.add(userMessageDTO);
                });
            } else {
                userDTOS.forEach(userDO -> {
                    UserMessageDTO userMessageDTO = new UserMessageDTO(userDO.getRealName(), userDO.getLoginName(), userDO.getRealName(), userDO.getImageUrl(), userDO.getEmail(), userDO.getLdap());
                    userMessageDTO.setCreationDate(userDO.getCreationDate());
                    userMessage.add(userMessageDTO);
                });
            }
        }
        return userMessage;
    }

    @Override
    public Page<UserMessageDTO> queryUserByProjectId(Long projectId, int page, int size, Boolean withLoginName) {
        Page<UserVO> pages = remoteIamOperator.queryUsersByProject(projectId, null, page, size);
        if (CollectionUtils.isEmpty(pages.getContent())) {
            return new Page<>();
        }
        List<UserMessageDTO> content = new ArrayList<>();
        if (withLoginName) {
            content = pages.getContent().stream()
                    .filter(v -> Boolean.TRUE.equals(v.getEnabled()))
                    .map(userDO -> {
                        String ldapName = userDO.getRealName() + "（" + userDO.getLoginName() + "）";
                        String noLdapName = userDO.getRealName() + "（" + userDO.getEmail() + "）";
                        UserMessageDTO userMessageDTO = new UserMessageDTO(userDO.getLdap() ? ldapName : noLdapName,
                                userDO.getLoginName(),
                                userDO.getRealName(),
                                userDO.getImageUrl(),
                                userDO.getEmail(),
                                userDO.getLdap(),
                                userDO.getId());
                        userMessageDTO.setCreationDate(userDO.getCreationDate());
                        return userMessageDTO;
                    }).collect(Collectors.toList());
        } else {
            content = pages.getContent().stream()
                    .filter(v -> Boolean.TRUE.equals(v.getEnabled()))
                    .map(userDO -> {
                        UserMessageDTO userMessageDTO = new UserMessageDTO(userDO.getRealName(), userDO.getLoginName(), userDO.getRealName(), userDO.getImageUrl(), userDO.getEmail(), userDO.getLdap());
                        userMessageDTO.setCreationDate(userDO.getCreationDate());
                        return userMessageDTO;
                    }).collect(Collectors.toList());
        }
        return PageUtil.buildPageInfoWithPageInfoList(pages, content);
    }

    @Override
    public Page<UserMessageDTO> queryUserByOrganizationId(Long organizationId, int page, int size, Boolean withLoginName) {
        Page<UserVO> pages = remoteIamOperator.queryUsersByOrganization(organizationId, null, page, size);
        if (CollectionUtils.isEmpty(pages.getContent())) {
            return new Page<>();
        }
        List<UserMessageDTO> content = new ArrayList<>();
        if (withLoginName) {
            content = pages.getContent().stream()
                    .filter(v -> Boolean.TRUE.equals(v.getEnabled()))
                    .map(userDO -> {
                        String ldapName = userDO.getRealName() + "（" + userDO.getLoginName() + "）";
                        String noLdapName = userDO.getRealName() + "（" + userDO.getEmail() + "）";
                        return new UserMessageDTO(userDO.getLdap() ? ldapName : noLdapName,
                                userDO.getLoginName(),
                                userDO.getRealName(),
                                userDO.getImageUrl(),
                                userDO.getEmail(),
                                userDO.getLdap(),
                                userDO.getId());
                    }).collect(Collectors.toList());
        } else {
            content = pages.getContent().stream()
                    .filter(v -> Boolean.TRUE.equals(v.getEnabled()))
                    .map(userDO -> new UserMessageDTO(userDO.getRealName(), userDO.getLoginName(), userDO.getRealName(), userDO.getImageUrl(), userDO.getEmail(), userDO.getLdap())).collect(Collectors.toList());
        }
        return PageUtil.buildPageInfoWithPageInfoList(pages, content);
    }

    @Override
    public List<UserVO> queryUsersByNameAndProjectId(Long projectId, String name) {
        return remoteIamOperator.list(projectId, name);
    }

    @Override
    public List<UserVO> listUsersByRealNames(List<String> realNames, boolean onlyEnabled) {
        if (ObjectUtils.isEmpty(realNames)) {
            return new ArrayList<>();
        } else {
            return remoteIamOperator.listUsersByRealNames(onlyEnabled, new HashSet<>(realNames));
        }
    }

    @Override
    public List<UserVO> listProjectAdminUsersByProjectId(Long projectId) {
        List<UserVO> users = remoteIamOperator.listProjectOwnerById(projectId);
        return CollectionUtils.isEmpty(users) ? new ArrayList<>() : users;
    }

    @Override
    public List<UserVO> listAllUsersByProject(Long projectId) {
        return remoteIamOperator.queryUsersByProject(projectId, null, 0, 0);
    }

    @Override
    public List<UserVO> listAllUsersByOrganization(Long organizationId) {
        return remoteIamOperator.queryUsersByOrganization(organizationId, null, 0, 0);
    }

    @Override
    public ProjectVO queryProject(Long projectId) {
        return ConvertUtil.queryProject(projectId);
    }

    @Override
    public List<RoleVO> listRolesWithUserCountOnProjectLevel(Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        return remoteIamOperator.listRolesWithUserCountOnProjectLevel(sourceId, roleAssignmentSearchVO);
    }

    @Override
    public Page<UserVO> pagingQueryUsersByRoleIdOnProjectLevel(int page, int size, Long roleId, Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        return remoteIamOperator.pagingQueryUsersByRoleIdOnProjectLevel(page, size, roleId, true, sourceId, roleAssignmentSearchVO);
    }

    @Override
    public List<UserDTO> listUsersByIds(Long[] ids) {
        return remoteIamOperator.listUsersByIds(ids, false);
    }

    @Override
    public boolean isProjectOwner(Long projectId, Long userId) {
        if (ObjectUtils.isEmpty(projectId)
                || ObjectUtils.isEmpty(userId)) {
            return false;
        }

        boolean isProjectOwner = remoteIamOperator.checkIsProjectOwner(userId, projectId);
        if (ObjectUtils.isEmpty(isProjectOwner)) {
            return false;
        } else {
            return isProjectOwner;
        }
    }


}
