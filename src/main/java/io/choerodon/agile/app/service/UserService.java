package io.choerodon.agile.app.service;

import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.RoleAssignmentSearchVO;
import io.choerodon.agile.api.vo.RoleVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;

import java.util.List;
import java.util.Map;

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

    Map<Long, UserMessageDTO> queryUserByProjectId(Long projectId, Boolean withLoginName);

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

    ProjectVO getGroupInfoByEnableProject(Long organizationId, Long projectId);

    /**
     * 查询是否为项目所有者
     *
     * @param projectId
     * @param userId
     * @return
     */
    boolean isProjectOwner(Long projectId, Long userId);

    List<UserVO> listUsersByRealNames(List<String> realNames, boolean onlyEnabled);

    List<UserVO> listProjectAdminUsersByProjectId(Long projectId);

    List<UserVO> listAllUsersByProject(Long projectId);

    List<UserVO> listAllUsersByOrganization(Long organizationId);
}
