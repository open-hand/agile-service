package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.KnowledgeRelationVO;
import io.choerodon.agile.api.vo.WikiRelationVO;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/12/03.
 * Email: fuqianghuang01@gmail.com
 */
public interface WikiRelationService {

    void create(Long projectId, List<WikiRelationVO> wikiRelationVOList);

    KnowledgeRelationVO queryByIssueId(Long projectId, Long issueId);

    void deleteById(Long projectId, Long id);

    void deleteByWorkSpaceId(Long projectId, Long workSpaceId);

    void copyIssueKnowledgeRelations(Long projectId, Long issueId, Long newIssueId);
}
