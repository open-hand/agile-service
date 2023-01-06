package io.choerodon.agile.app.service.impl;

import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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

import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

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
    @Transactional(rollbackFor = Exception.class)
    public WorkLogDTO createBase(WorkLogDTO workLogDTO) {
        if (workLogMapper.insert(workLogDTO) != 1) {
            throw new CommonException("error.workLog.insert");
        }
        return workLogMapper.selectByPrimaryKey(workLogDTO.getLogId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteByProjectIdWithoutDataLog(Long projectId, Long issueId) {
        if(projectId == null || issueId == null) {
            return;
        }
        final List<WorkLogDTO> workLogs = this.workLogMapper.selectByCondition(Condition.builder(WorkLogDTO.class).andWhere(Sqls.custom()
                .andEqualTo(WorkLogDTO.FIELD_PROJECT_ID, projectId)
                .andEqualTo(WorkLogDTO.FIELD_ISSUE_ID, issueId)
        ).build());
        if(CollectionUtils.isEmpty(workLogs)) {
            return;
        }
        for (WorkLogDTO workLog : workLogs) {
            this.innerDelete(projectId, workLog.getLogId());
        }
    }

    @Override
    @DataLog(type = "deleteWorkLog")
    @Transactional(rollbackFor = Exception.class)
    public void deleteBase(Long projectId,Long logId) {
        this.innerDelete(projectId, logId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteBaseWithoutDataLog(Long projectId, Long logId) {
        this.innerDelete(projectId, logId);
    }

    /**
     * 根据项目ID和工时日志ID删除工时日志
     * @param projectId 项目ID
     * @param logId     工时日志ID
     */
    private void innerDelete(Long projectId,Long logId) {
        if(projectId == null || logId == null) {
            return;
        }
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
