package io.choerodon.agile.infra.repository.impl;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.domain.repository.IssueRepository;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;

/**
 * @author superlee
 * @since 2022-08-24
 */
@Repository
public class IssueRepositoryImpl implements IssueRepository {

    @Autowired
    private IssueMapper issueMapper;

    @Override
    public List<IssueDTO> selectByIds(Set<Long> ids) {
        if (ObjectUtils.isEmpty(ids)) {
            return Collections.emptyList();
        }
        return issueMapper.selectByIds(StringUtils.join(ids, ","));
    }

    @Override
    public IssueDTO queryIssueBySummary(Long projectId, String summary) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setSummary(summary);
        return issueMapper.selectOne(issueDTO);
    }
}
