package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.RoleAssignmentSearchVO;
import io.choerodon.agile.api.vo.RoleVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.core.domain.Page;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/28
 */
public interface UserService {

    /**
     * 查询
     *
     * @param userId userId
     * @param withId withId
     * @return userDO
     */
    UserDTO queryUserNameByOption(Long userId, Boolean withId);

    Map<Long, UserMessageDTO> queryUsersMap(List<Long> assigneeIds, Boolean withLoginName);

    List<UserMessageDTO> queryUsers(List<Long> assigneeIds, Boolean withLoginName);

    Page<UserMessageDTO> queryUserByProjectId(Long projectId, int page, int size, Boolean withLoginName);

    Page<UserMessageDTO> queryUserByOrganizationId(Long organizationId, int page, int size, Boolean withLoginName);

    /**
     * 根据项目id和名称查询用户信息
     *
     * @param projectId projectId
     * @param name      name
     * @return UserVO
     */
    List<UserVO> queryUsersByNameAndProjectId(Long projectId, String name);


    /**
     * 根据项目id查询项目信息
     *
     * @param projectId projectId
     * @return ProjectVO
     */
    ProjectVO queryProject(Long projectId);

    List<RoleVO> listRolesWithUserCountOnProjectLevel(Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO);

    Page<UserVO> pagingQueryUsersByRoleIdOnProjectLevel(int page, int size, Long roleId, Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO);

    List<UserDTO> listUsersByIds(Long[] ids);

    /**
     * 查询是否为项目所有者
     *
     * @param projectId projectId
     * @param userId userId
     * @return result
     */
    boolean isProjectOwner(Long projectId, Long userId);

    List<UserVO> listUsersByRealNames(List<String> realNames, boolean onlyEnabled);

    List<UserVO> listProjectAdminUsersByProjectId(Long projectId);

    List<UserVO> listAllUsersByProject(Long projectId);

    List<UserVO> listAllUsersByOrganization(Long organizationId);
}
