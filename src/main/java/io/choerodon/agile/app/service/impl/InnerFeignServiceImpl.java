package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.InnerFeignService;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-04-15
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class InnerFeignServiceImpl implements InnerFeignService {

    @Autowired
    private IssueMapper issueMapper;

    @Override
    public List<IssueDTO> listIssueByIds(List<Long> issueIds) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return new ArrayList<>();
        }
        Set<Long> issueIdSet = new HashSet<>(issueIds);
        return issueMapper.selectByIds(StringUtils.join(issueIdSet, ","));
    }
}
