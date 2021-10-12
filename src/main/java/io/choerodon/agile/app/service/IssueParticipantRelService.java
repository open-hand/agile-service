package io.choerodon.agile.app.service;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-12 15:21
 */
public interface IssueParticipantRelService {
    void createParticipantRel(Long issueId, Long projectId, List<Long> participantIds);

    void updateParticipantRel(Long issueId, Long projectId, List<Long> participantIds);

    void deleteParticipantRel(Long issueId, Long projectId);
}
