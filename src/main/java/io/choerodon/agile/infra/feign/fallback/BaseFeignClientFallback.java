package io.choerodon.agile.infra.feign.fallback;

import javax.validation.Valid;
import java.util.List;
import java.util.Set;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.TimeZoneWorkCalendarDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/24
 */
@Component
public class BaseFeignClientFallback implements BaseFeignClient {

    private static final String QUERY_ERROR = "error.UserFeign.query";
    private static final String BATCH_QUERY_ERROR = "error.UserFeign.queryList";

    @Override
    public ResponseEntity<UserDTO> query(Long organizationId, Long id) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<List<UserDTO>> listUsersByIds(Long[] ids, Boolean onlyEnabled) {
        throw new CommonException(BATCH_QUERY_ERROR);
    }

    @Override
    public ResponseEntity<ProjectVO> queryProject(Long id) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<ProjectVO> queryProject(Long id, Boolean withAgileInfo) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<Page<UserDTO>> listUsersByProjectId(Long id, int page, int size, String param) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<Page<UserDTO>> agileUsers(Long projectId, int page, int size, AgileUserVO agileUserVO) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<Page<UserVO>> list(Long id, String param) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<List<RoleVO>> listRolesWithUserCountOnProjectLevel(Long sourceId, RoleAssignmentSearchVO roleAssignmentSearchVO) {
        throw new CommonException(QUERY_ERROR);
    }

    @Override
    public ResponseEntity<Page<UserVO>> pagingQueryUsersByRoleIdOnProjectLevel(int page,
                                                                               int size,
                                                                               Long roleId,
                                                                               Boolean doPage,
                                                                               Long sourceId,
                                                                               RoleAssignmentSearchVO roleAssignmentSearchVO) {
        throw new CommonException("error.users.get");
    }

    @Override
    public ResponseEntity<ProjectVO> getGroupInfoByEnableProject(Long organizationId, Long projectId) {
        throw new CommonException("error.groupInfo.get");
    }

    @Override
    public ResponseEntity<Page<UserWithRoleVO>> pagingQueryUsersWithProjectLevelRoles(int page, int size, Long sourceId, @Valid RoleAssignmentSearchVO roleAssignmentSearchVO, boolean doPage) {
        throw new CommonException("error.usersWithRoles.get");
    }

    @Override
    public ResponseEntity<List<ProjectVO>> listProjectsByOrgId(Long organizationId) {
        throw new CommonException("error.iamServiceFeignFallback.listProjectsByOrgId");
    }

    @Override
    public ResponseEntity<Page<ProjectVO>> listWithCategoryByOrganizationIds(Long organizationId,
                                                                             ProjectSearchVO projectSearchVO,
                                                                             Integer page,
                                                                             Integer size) {
        throw new CommonException("error.iamServiceFeignFallback.listWithCategoryByOrganizationIds");
    }


    @Override
    public ResponseEntity<TimeZoneWorkCalendarDTO> queryTimeZoneDetailByOrganizationId(Long organizationId) {
        throw new CommonException("error.iamServiceFeignFallback.queryTimeZoneDetailByOrganizationId");
    }

    @Override
    public ResponseEntity<List<WorkCalendarHolidayRefVO>> queryWorkCalendarHolidayRelByYear(Long organizationId, Integer year) {
        throw new CommonException("error.iamServiceFeignFallback.queryWorkCalendarHolidayRelByYear");
    }

    @Override
    public ResponseEntity<List<WorkCalendarHolidayRefVO>> queryByYearIncludeLastAndNext(Long organizationId, Integer year) {
        throw new CommonException("error.iamServiceFeignFallback.queryByYearIncludeLastAndNext");
    }

    @Override
    public ResponseEntity<List<RoleVO>> getUserWithProjLevelRolesByUserId(Long projectId, Long userId) {
        throw new CommonException("error.iamServiceFeignFallback.getUserWithProjLevelRolesByUserId");
    }

    @Override
    public ResponseEntity<List<UserVO>> listUsersByRealNames(Boolean onlyEnabled, Set<String> realNames) {
        throw new CommonException("error.iamServiceFeignFallback.listUsersByRealNames");
    }

    @Override
    public ResponseEntity<List<UserVO>> listProjectOwnerById(Long projectId) {
        throw new CommonException("error.query.project.admin");
    }

    @Override
    public ResponseEntity<Boolean> checkIsProjectOwner(Long id, Long projectId) {
        throw new CommonException("error.check.project.admin");
    }

    @Override
    public ResponseEntity<List<ProjectVO>> queryProjects(Long id, boolean includedDisabled) {
        throw new CommonException("error.query.project");
    }

    @Override
    public ResponseEntity<List<ProjectVO>> queryOrgProjects(Long organizationId, Long userId) {
        throw new CommonException("error.list.projects");
    }

    @Override
    public ResponseEntity<Page<ProjectVO>> pagingProjectsByUserId(Long organizationId, Long userId, int page, int size, Boolean enabled, String category) {
        throw new CommonException("error.list.projects");
    }

    @Override
    public ResponseEntity<List<ProjectVO>> queryByIds(Set<Long> ids) {
        throw new CommonException("error.list.projects");
    }

    @Override
    public ResponseEntity<List<RoleVO>> listProjectRoles(Long projectId,
                                                         Boolean onlySelectEnable,
                                                         String roleName) {
        throw new CommonException("error.list.project.roles");
    }

    @Override
    public ResponseEntity<Page<UserVO>> queryUsersByProject(Long projectId, String param, int page, int size) {
        throw new CommonException("error.list.project.users");
    }

    @Override
    public ResponseEntity<Page<UserVO>> queryUsersByOrganization(Long projectId, String param, int page, int size) {
        throw new CommonException("error.list.organization.users");
    }

    @Override
    public ResponseEntity<List<ProjectWithUserVO>> listProjectOwnerByIds(Set<Long> projectIds) {
        throw new CommonException("error.listProjectOwnerByIds");
    }

    @Override
    public ResponseEntity<OrganizationInfoVO> query(Long id) {
        throw new CommonException("error.organization.query");
    }

    @Override
    public ResponseEntity<Page<UserDTO>> agileUsersByProjectIds(Long projectId, int page, int size, AgileUserVO agileUserVO) {
        throw new CommonException("error.agile.user.by.projects");
    }

    @Override
    public ResponseEntity<List<RoleVO>> listRolesByIds(Long tenantId, List<Long> roleIds) {
        throw new CommonException("error.listRolesByIds");
    }

    @Override
    public ResponseEntity<Page<ProjectVO>> pagedQueryProjects(Long organizationId,
                                                              int page,
                                                              int size,
                                                              String name,
                                                              String code,
                                                              Boolean enabled,
                                                              Boolean withAdditionInfo,
                                                              String params) {
        throw new CommonException("error.query.projects.under.organization");
    }

    @Override
    public ResponseEntity<List<ProjectVO>> queryProjectByIds(Set<Long> projectIds) {
        throw new CommonException("error.query.project.by.ids");
    }
}
