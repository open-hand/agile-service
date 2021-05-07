package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.business.IssueDTO;

import java.util.List;

/**
 * @author superlee
 * @since 2021-04-15
 */
public interface InnerFeignService {

    List<IssueDTO> listIssueByIds(List<Long> issueIds);
}
