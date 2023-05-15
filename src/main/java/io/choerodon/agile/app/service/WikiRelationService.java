package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.WikiRelationVO;
import io.choerodon.agile.infra.dto.WikiRelationDTO;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/12/03.
 * Email: fuqianghuang01@gmail.com
 */
public interface WikiRelationService {

    void create(Long projectId, List<WikiRelationVO> wikiRelationVOList);

    List<WikiRelationVO> queryByIssueId(Long projectId, Long issueId);

    List<WikiRelationDTO> baseQueryByIssueIdAndType(Long issueId, String type);

    void deleteById(Long projectId, Long id);

    void deleteByWorkSpaceId(Long projectId, Long workSpaceId);

    void copyIssueKnowledgeRelations(Long projectId, Long issueId, Long newIssueId);
}
