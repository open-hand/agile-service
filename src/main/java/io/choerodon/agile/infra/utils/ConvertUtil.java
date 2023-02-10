package io.choerodon.agile.infra.utils;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSON;
import org.springframework.util.CollectionUtils;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.InstanceService;
import io.choerodon.agile.app.service.PriorityService;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.app.service.StatusService;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.core.exception.CommonException;

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

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     * @param projectId
     * @return
     */
    public static Long getOrganizationId(Long projectId) {
        return queryProject(projectId, false).getOrganizationId();
    }

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     * @param projectId
     * @return
     */
    public static String getCode(Long projectId) {
        return queryProject(projectId, false).getCode();
    }

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     * @param projectId
     * @return
     */
    public static String getName(Long projectId) {
        return queryProject(projectId, false).getName();
    }

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     * 获取项目信息, 优先从缓存里获取, 取不到再去cbase取
     * @param projectId                项目ID
     * @return 查询结果
     */
    public static ProjectVO queryProject(Long projectId) {
        return queryProject(projectId, true);
    }

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     *
     * 获取项目信息, 优先从缓存里获取, 取不到再去cbase取
     * @param projectId                项目ID
     * @param checkProjectInfIntact    是否校验cbase返回的数据完整
     * @return 查询结果
     */
    public static ProjectVO queryProject(Long projectId, boolean checkProjectInfIntact) {
        if (Objects.isNull(projectId)) {
            throw new CommonException("error.projectId.not.null");
        }
        // 先从缓存取
        RedisUtil redisUtil = SpringBeanUtil.getBean(RedisUtil.class);
        String key = "projectInfo:" + projectId;
        Object obj = redisUtil.get(key);
        ProjectVO projectVO = null;
        if (obj != null) {
            projectVO = JSON.parseObject(obj.toString(), ProjectVO.class);
            // 有的时候缓存里的数据是残缺的, 需要做一些数据校验,
            // 如果数据有问题, 则清空这个有问题的缓存, 从CBASE重新加载
            if (projectVO.getId() == null || CollectionUtils.isEmpty(projectVO.getCategories())) {
                redisUtil.delete(key);
                projectVO = null;
            }
        }
        // 缓存取不到从CBASE取
        if (projectVO == null) {
            projectVO = SpringBeanUtil.getBeansOfSuper(RemoteIamOperator.class).queryProject(projectId);
            if (projectVO == null) {
                throw new CommonException("error.queryProject.notFound");
            }
            if(checkProjectInfIntact) {
                // 有的时候CBASE会返回残缺的数据, 这里需要校验CBASE返回值合法性
                if (projectVO.getId() == null) {
                    throw new CommonException("error.queryProject.notFound");
                } else if (CollectionUtils.isEmpty(projectVO.getCategories())) {
                    throw new CommonException("error.queryProject.categories.init");
                }
            }
            // 只有完整的数据才能写入缓存
            if(projectVO.getId() != null && !CollectionUtils.isEmpty(projectVO.getCategories())) {
                redisUtil.set(key, JSON.toJSONString(projectVO));
            }
        }
        return projectVO;
    }

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     * @param projectId
     * @param code
     * @return
     */
    public static Boolean hasModule(Long projectId, String code) {
        ProjectVO projectVO= ConvertUtil.queryProject(projectId, false);
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
