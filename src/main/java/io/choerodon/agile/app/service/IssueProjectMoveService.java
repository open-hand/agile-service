package io.choerodon.agile.app.service;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.api.vo.ProjectVO;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.List;
import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2021-01-05 13:36
 */
public interface IssueProjectMoveService {
    /**
     * issue跨项目转交
     * @param projectId
     * @param issueId
     * @param targetProjectId
     * @param jsonObject
     */
    void issueProjectMove(Long projectId, Long issueId, Long targetProjectId, JSONObject jsonObject);

    /**
     * 查询能进行移动的项目
     * @param projectId
     * @param typeCode
     * @return
     */
    List<ProjectVO> listMoveProject(Long projectId, String typeCode);

    /**
     * 查询issue移动到其他项目会丢失的自定义字段
     * @param projectId
     * @param issueId
     * @param targetProject
     * @param issueTypeId
     * @return
     */
    List<ObjectSchemeFieldVO> listLostField(Long projectId, Long issueId, Long targetProject, Long issueTypeId);

    /**
     * 跨项目批量移动
     * @param projectId
     * @param targetProjectId
     * @param jsonObject
     * @param requestAttributes
     * @param encryptType
     */
    void issueProjectBatchMove(Long projectId,
                               Long targetProjectId,
                               JSONObject jsonObject,
                               ServletRequestAttributes requestAttributes,
                               String encryptType);

    void handlerIssueValue(ProjectVO projectVO, Long issueId, ProjectVO targetProjectVO, JSONObject jsonObject);

    /**
     * 根据issueIds查询并统计问题类型下的状态
     * @param projectId
     * @param issueIds
     * @return
     */
    Map<String, List<String>> issueTypeStatusMap(Long projectId, List<Long> issueIds);
}
