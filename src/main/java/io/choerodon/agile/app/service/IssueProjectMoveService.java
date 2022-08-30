package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;

import com.alibaba.fastjson.JSONObject;
import org.springframework.web.context.request.ServletRequestAttributes;

import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.api.vo.ProjectVO;

/**
 * @author zhaotianxin
 * @date 2021-01-05 13:36
 */
public interface IssueProjectMoveService {
    /**
     * issue跨项目转交
     * @param projectId projectId
     * @param issueId issueId
     * @param targetProjectId targetProjectId
     * @param jsonObject jsonObject
     */
    void issueProjectMove(Long projectId, Long issueId, Long targetProjectId, JSONObject jsonObject);

    /**
     * 查询能进行移动的项目
     * @param projectId projectId
     * @param typeCode typeCode
     * @return result
     */
    List<ProjectVO> listMoveProject(Long projectId, String typeCode);

    /**
     * 查询issue移动到其他项目会丢失的自定义字段
     * @param projectId projectId
     * @param issueId issueId
     * @param targetProject targetProject
     * @param issueTypeId issueTypeId
     * @return result
     */
    List<ObjectSchemeFieldVO> listLostField(Long projectId, Long issueId, Long targetProject, Long issueTypeId);

    /**
     * 跨项目批量移动
     * @param projectId projectId
     * @param targetProjectId targetProjectId
     * @param jsonObject jsonObject
     * @param requestAttributes requestAttributes
     * @param encryptType encryptType
     */
    void issueProjectBatchMove(Long projectId,
                               Long targetProjectId,
                               JSONObject jsonObject,
                               ServletRequestAttributes requestAttributes,
                               String encryptType);

    void handlerIssueValue(ProjectVO projectVO, Long issueId, ProjectVO targetProjectVO, JSONObject jsonObject);

    /**
     * 根据issueIds查询并统计问题类型下的状态
     * @param projectId projectId
     * @param issueIds issueIds
     * @return result
     */
    Map<String, List<String>> issueTypeStatusMap(Long projectId, List<Long> issueIds);
}
