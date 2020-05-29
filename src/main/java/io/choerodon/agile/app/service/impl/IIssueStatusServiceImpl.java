package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.IIssueStatusService;
import io.choerodon.agile.infra.annotation.DataLog;
import io.choerodon.agile.infra.aspect.DataLogRedisUtil;
import io.choerodon.agile.infra.dto.IssueStatusDTO;
import io.choerodon.agile.infra.mapper.IssueStatusMapper;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


@Service
public class IIssueStatusServiceImpl implements IIssueStatusService {

    @Autowired
    private IssueStatusMapper issueStatusMapper;

    @Autowired
    private DataLogRedisUtil dataLogRedisUtil;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    @DataLog(type = "batchUpdateIssueStatus", single = false)
    public IssueStatusDTO update(IssueStatusDTO issueStatusDTO) {
        if (issueStatusMapper.updateByPrimaryKeySelective(issueStatusDTO) != 1) {
            throw new CommonException("error.status.update");
        }
        dataLogRedisUtil.deleteByUpdateIssueStatus(issueStatusDTO);
        return modelMapper.map(issueStatusMapper.selectByStatusId(issueStatusDTO.getProjectId(), issueStatusDTO.getStatusId()), IssueStatusDTO.class);
    }
}
