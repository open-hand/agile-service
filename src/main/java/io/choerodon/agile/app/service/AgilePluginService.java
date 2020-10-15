package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.QuickFilterValueVO;
import io.choerodon.agile.infra.dto.IssueConvertDTO;
import io.choerodon.agile.infra.dto.PageFieldDTO;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-10-12 10:35
 */
public interface AgilePluginService {
    String getSystemFieldContext(String code);

    void deleteIssueForBusiness(IssueConvertDTO issueConvertDTO);

    void appendProgramFieldSql(StringBuilder sqlQuery, QuickFilterValueVO quickFilterValueVO, String value, String operation,Long projectId);

    void handlerFeatureRank(Long projectId, String type);

    void getProgramEpicIds(List<Long> epicIds, Long projectId);

    List<PageFieldDTO> handlerProgramPageField(Long projectId, String issueType, List<PageFieldDTO> pageFields);
}
