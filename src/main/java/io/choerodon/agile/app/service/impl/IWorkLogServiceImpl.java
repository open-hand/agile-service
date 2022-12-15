package io.choerodon.agile.app.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import io.choerodon.agile.app.service.IWorkLogService;
import io.choerodon.agile.infra.annotation.DataLog;
import io.choerodon.agile.infra.dto.WorkLogDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.WorkLogMapper;
import io.choerodon.agile.infra.utils.BaseFieldUtil;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/9/5.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class IWorkLogServiceImpl implements IWorkLogService {

    @Autowired
    private WorkLogMapper workLogMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private SendMsgUtil messageUtil;

    @Override
    @DataLog(type = "createWorkLog")
    public WorkLogDTO createBase(WorkLogDTO workLogDTO) {
        if (workLogMapper.insert(workLogDTO) != 1) {
            throw new CommonException("error.workLog.insert");
        }
        return workLogMapper.selectByPrimaryKey(workLogDTO.getLogId());
    }

    @Override
    @DataLog(type = "deleteWorkLog")
    public void deleteBase(Long projectId,Long logId) {
        WorkLogDTO query = new WorkLogDTO();
        query.setProjectId(projectId);
        query.setLogId(logId);
        WorkLogDTO workLogDTO = workLogMapper.selectOne(query);
        if (workLogDTO == null) {
            throw new CommonException("error.workLog.get");
        }
        final Long issueId = workLogDTO.getIssueId();
        if (workLogMapper.delete(workLogDTO) != 1) {
            throw new CommonException("error.workLog.delete");
        }
        BaseFieldUtil.updateIssueLastUpdateInfo(issueId, workLogDTO.getProjectId());
        final IssueDetailDTO issue = this.issueMapper.queryIssueDetail(projectId, issueId);
        messageUtil.sendMsgByWorkLogDelete(projectId, issue, workLogDTO, DetailsHelper.getUserDetails());
    }

}
