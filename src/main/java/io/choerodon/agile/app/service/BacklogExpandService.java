package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.PageConfigFieldEditedVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import org.apache.commons.collections.map.MultiKeyMap;

import java.util.List;
import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2020-09-22 16:27
 */
public interface BacklogExpandService {

   void deleteIssueBacklogRel(Long issueId);

   void changeDetection(Long issueId,Long projectId,Long organizationId);

   Boolean enabled(Long projectId);

   Map<String, PageConfigFieldEditedVO> fieldEdited(String issueType);

   String getSystemFieldContext(String code);

    void processBacklogFields(Long editPageId, MultiKeyMap dataMap, MultiKeyMap rankMap, List<ObjectSchemeFieldDTO> fields);
}
