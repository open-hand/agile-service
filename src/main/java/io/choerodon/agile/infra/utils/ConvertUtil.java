package io.choerodon.agile.infra.utils;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSON;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.InstanceService;
import io.choerodon.agile.app.service.PriorityService;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.app.service.StatusService;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.CollectionUtils;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/10/31
 */
public class ConvertUtil {

    private ConvertUtil() {
        throw new IllegalStateException("Utility class");
    }
    /**
     * 根据projectId获取issue类型Map
     *
     * @param projectId projectId
     * @return IssueTypeMap
     */
    public static Map<Long, IssueTypeVO> getIssueTypeMap(Long projectId, String applyType) {
        List<IssueTypeVO> issueTypeVOS = SpringBeanUtil.getBean(ProjectConfigService.class).queryIssueTypesByProjectId(projectId, applyType, false);
        return issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
    }

    /**
     * 根据projectId获取issue状态Map
     *
     * @param projectId projectId
     * @return StatusMap
     */
    public static Map<Long, StatusVO> getIssueStatusMap(Long projectId) {
        Long organizationId = getOrganizationId(projectId);
        return SpringBeanUtil.getBean(StatusService.class).queryAllStatusMap(organizationId);
    }

    /**
     * 根据projectId获取issue优先级Map
     *
     * @param projectId projectId
     * @return PriorityDTOMap
     */
    public static Map<Long, PriorityVO> getIssuePriorityMap(Long projectId) {
        Long organizationId = getOrganizationId(projectId);
        return SpringBeanUtil.getBean(PriorityService.class).queryByOrganizationId(organizationId);
    }

    public static Long getOrganizationId(Long projectId) {
        return queryProject(projectId).getOrganizationId();
    }

    public static String getCode(Long projectId) {
        return queryProject(projectId).getCode();
    }

    public static String getName(Long projectId) {
        return queryProject(projectId).getName();
    }

    public static ProjectVO queryProject(Long projectId) {
        if (Objects.isNull(projectId)) {
            throw new CommonException("error.projectId.not.null");
        }
        RedisUtil redisUtil = SpringBeanUtil.getBean(RedisUtil.class);
        String key = "projectInfo:"+projectId;
        Object project = redisUtil.get(key);
        if (project != null) {
            ProjectVO projectVO = JSON.parseObject(project.toString(), ProjectVO.class);
            if (projectVO.getId() == null) {
                redisUtil.delete(key);
                throw new CommonException("error.queryProject.notFound");
            } else {
                return projectVO;
            }
        } else {
            ProjectVO projectVO = SpringBeanUtil.getBean(RemoteIamOperator.class).queryProject(projectId);
            if (projectVO != null) {
                if (projectVO.getId() == null) {
                    throw new CommonException("error.queryProject.notFound");
                }
                redisUtil.set(key, JSON.toJSONString(projectVO));
                return projectVO;
            } else {
                throw new CommonException("error.queryProject.notFound");
            }
        }
    }

    public static Boolean hasModule(Long projectId, String code) {
        ProjectVO projectVO= ConvertUtil.queryProject(projectId);
        List<ProjectCategoryDTO> categories = projectVO.getCategories();
        if(CollectionUtils.isEmpty(categories)){
            return false;
        }
        List<String> codes = categories.stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toList());
        return codes.contains(code);
    }

    public static Map<Long, IssueTypeWithStateMachineIdVO> queryIssueTypesWithStateMachineIdByProjectId(Long projectId, String applyType) {
        List<IssueTypeWithStateMachineIdVO> issueTypeWithStateMachineIdVOS = SpringBeanUtil.getBean(ProjectConfigService.class).queryIssueTypesWithStateMachineIdByProjectId(projectId, applyType, false);
        Map<Long, Long> statusIdMap = SpringBeanUtil.getBean(InstanceService.class).queryInitStatusIds(getOrganizationId(projectId), issueTypeWithStateMachineIdVOS
                .stream().map(IssueTypeWithStateMachineIdVO::getStateMachineId).collect(Collectors.toList()));
        issueTypeWithStateMachineIdVOS.forEach(issueTypeWithStateMachineIdDTO -> issueTypeWithStateMachineIdDTO.setInitStatusId(statusIdMap.get(issueTypeWithStateMachineIdDTO.getStateMachineId())));
        return issueTypeWithStateMachineIdVOS.stream().collect(Collectors.toMap(IssueTypeWithStateMachineIdVO::getId, Function.identity()));
    }


}
