package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssueParticipantRelDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-12 14:11
 */
public interface IssueParticipantRelMapper extends BaseMapper<IssueParticipantRelDTO> {
    void batchDeleteByIssueId(@Param("issueId") Long issueId);

    void deleteByIssueIdAndParticipantIds(@Param("projectId") Long projectId, @Param("issueId") Long issueId, @Param("participantIds")  List<Long> participantIds);

    List<Long> listByIssueId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);
}
