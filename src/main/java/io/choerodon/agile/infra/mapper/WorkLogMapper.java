package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.IssueWorkTimeCountVO;
import io.choerodon.agile.api.vo.WorkLogVO;
import io.choerodon.agile.infra.dto.WorkLogDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;
import java.util.Set;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/18.
 * Email: fuqianghuang01@gmail.com
 */
public interface WorkLogMapper extends BaseMapper<WorkLogDTO> {

    /**
     * 根据issueId 倒序查找WorkLog
     *
     * @param issueId   issueId
     * @param projectId projectId
     * @return WorkLogDTO
     */
    List<WorkLogDTO> queryByIssueId(@Param("issueId") Long issueId, @Param("projectId") Long projectId);

    List<WorkLogDTO> selectWorkTimeBySpring(@Param("projectId") Long projectId,
                                            @Param("sprintId") Long springId,
                                            @Param("startDate") Date startDate,
                                            @Param("endDate") Date endDate);

    void updateProject(@Param("projectId") Long projectId, @Param("issueId") Long issueId, @Param("targetProjectId") Long targetProjectId);

    List<WorkLogVO> queryByIssueIds(@Param("projectIds") List<Long> projectIds, @Param("issueIds") List<Long> issueIds);

    List<WorkLogVO> selectTotalWorkTimeByIssueId(@Param("projectIds") Set<Long> projectIds,
                                                 @Param("issueIds") List<Long> issueIds);

    IssueWorkTimeCountVO countWorkTime(@Param("issueId") Long issueId, @Param("projectId") Long projectId);
}
