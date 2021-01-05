package io.choerodon.agile.app.service;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.api.vo.ProjectVO;

import java.util.List;

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
     * @param typeCode
     * @return
     */
    List<ObjectSchemeFieldVO> listLostField(Long projectId, Long issueId, Long targetProject, String typeCode);
}
