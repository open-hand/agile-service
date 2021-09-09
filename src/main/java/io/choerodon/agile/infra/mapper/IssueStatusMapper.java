package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.api.vo.event.AddStatusWithProject;
import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.agile.infra.dto.IssueStatusDTO;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
public interface IssueStatusMapper extends BaseMapper<IssueStatusDTO> {

    List queryUnCorrespondStatus(@Param("projectId") Long projectId, @Param("boardId") Long boardId, @Param("realStatusIds") List<Long> realStatusIds);

    IssueStatusDTO selectByStatusId(@Param("projectId") Long projectId, @Param("statusId") Long statusId);

    /**
     * 批量创建状态
     *
     * @param addStatusWithProjects addStatusWithProjects
     * @param userId                userId
     */
    void batchCreateStatusByProjectIds(@Param("addStatusWithProjects") List<AddStatusWithProject> addStatusWithProjects, @Param("userId") Long userId);

    List<StatusVO> listWithCompleted(@Param("projectId") Long projectId, @Param("organizationId") Long organizationId);

    List<StatusVO> listCompletedStatus(@Param("projectIds") Set<Long> projectIds);

    /**
     * 根据状态id查询项目下状态
     * @param projectId 项目id
     * @param organizationId 组织id
     * @param statusIds 状态id
     * @return 项目下状态
     */
    List<StatusVO> listStatusByIds(@Param("projectId") Long projectId, @Param("organizationId") Long organizationId, @Param("statusIds") List<Long> statusIds);

    List<Long> queryUnCompletedStatus(@Param("projectId") Long projectId);
}
