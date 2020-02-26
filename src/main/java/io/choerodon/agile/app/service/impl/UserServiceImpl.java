package io.choerodon.agile.app.service.impl;

import com.github.pagehelper.PageInfo;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.RoleAssignmentSearchVO;
import io.choerodon.agile.api.vo.RoleVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/28
 */
@Component
public class UserServiceImpl implements UserService {

    private final BaseFeignClient baseFeignClient;

    @Autowired
    public UserServiceImpl(BaseFeignClient baseFeignClient) {
        this.baseFeignClient = baseFeignClient;
    }

    @Override
    public UserDTO queryUserNameByOption(Long userId, Boolean withId) {
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        if (userId == null || userId == 0) {
            return new UserDTO();
        } else {
            UserDTO userDTO = baseFeignClient.query(customUserDetails.getOrganizationId(), userId).getBody();
            if (withId) {
                userDTO.setRealName(userDTO.getLoginName() + userDTO.getRealName());
                return userDTO;
            } else {
                return userDTO;
            }
        }
    }

    @Override
    public Map<Long, UserMessageDTO> queryUsersMap(List<Long> assigneeIdList, Boolean withLoginName) {
        if (assigneeIdList == null) {
            return new HashMap<>();
        }
        Map<Long, UserMessageDTO> userMessageMap = new HashMap<>(assigneeIdList.size());
        if (!assigneeIdList.isEmpty()) {
            Long[] assigneeIds = new Long[assigneeIdList.size()];
            assigneeIdList.toArray(assigneeIds);
            List<UserDTO> userDTOS = baseFeignClient.listUsersByIds(assigneeIds, false).getBody();
            if (withLoginName) {
                userDTOS.forEach(userDO -> {
                    String ldapName = userDO.getRealName() + "（" + userDO.getLoginName() + "）";
                    String noLdapName = userDO.getRealName() + "（" + userDO.getEmail() + "）";
                    userMessageMap.put(userDO.getId(),
                            new UserMessageDTO(userDO.getLdap() ? ldapName : noLdapName,
                                    userDO.getLoginName(),
                                    userDO.getRealName(),
                                    userDO.getImageUrl(),
                                    userDO.getEmail(),
                                    userDO.getLdap(),
                                    userDO.getId()));
                });
            } else {
                userDTOS.forEach(userDO -> userMessageMap.put(userDO.getId(), new UserMessageDTO(userDO.getRealName(), userDO.getLoginName(), userDO.getRealName(), userDO.getImageUrl(), userDO.getEmail(), userDO.getLdap())));
            }
        }
        return userMessageMap;
    }

    @Override
    public List<UserVO> queryUsersByNameAndProjectId(Long projectId, String name) {
        ResponseEntity<PageInfo<UserVO>> userList = baseFeignClient.list(projectId, name);
        if (userList != null) {
            return userList.getBody().getList();
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public ProjectVO queryProject(Long projectId) {
        return baseFeignClient.queryProject(projectId).getBody();
    }

    @Override
    public List<RoleVO> listRolesWithUserCountOnProjectLevel(Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        ResponseEntity<List<RoleVO>> roles = baseFeignClient.listRolesWithUserCountOnProjectLevel(sourceId, roleAssignmentSearchVO);
        return roles != null ? roles.getBody() : new ArrayList<>();
    }

    @Override
    public PageInfo<UserVO> pagingQueryUsersByRoleIdOnProjectLevel(int page, int size, Long roleId, Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        ResponseEntity<PageInfo<UserVO>> users = baseFeignClient.pagingQueryUsersByRoleIdOnProjectLevel(page, size, roleId, sourceId, roleAssignmentSearchVO);
        return users != null ? users.getBody() : new PageInfo<>(new ArrayList<>());
    }

    @Override
    public List<UserDTO> listUsersByIds(Long[] ids) {
        ResponseEntity<List<UserDTO>> users = baseFeignClient.listUsersByIds(ids, false);
        return users != null ? users.getBody() : new ArrayList<>();
    }

    @Override
    public ProjectVO getGroupInfoByEnableProject(Long organizationId, Long projectId) {
        ResponseEntity<ProjectVO> projectDTOResponseEntity = baseFeignClient.getGroupInfoByEnableProject(ConvertUtil.getOrganizationId(projectId), projectId);
        return projectDTOResponseEntity != null ? projectDTOResponseEntity.getBody() : null;
    }

}
