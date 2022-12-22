package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.IssueWorkTimeCountVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.WorkLogVO;
import io.choerodon.agile.api.validator.WorkLogValidator;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.WorkLogDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.enums.RuleNoticeEvent;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.WorkLogMapper;
import io.choerodon.agile.infra.utils.BaseFieldUtil;
import io.choerodon.core.exception.CommonException;
import org.hzero.core.base.AopProxy;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/18.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkLogServiceImpl implements WorkLogService, AopProxy<WorkLogService> {

    private static final String SELF_ADJUSTMENT = "self_adjustment";
    private static final String NO_SET_PREDICTION_TIME = "no_set_prediction_time";
    private static final String SET_TO = "set_to";
    private static final String REDUCE = "reduce";
    private static final String REMAINING_TIME_FIELD = "remainingTime";

    @Autowired
    private WorkLogMapper workLogMapper;

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private UserService userService;

    @Autowired
    private IssueAccessDataService issueAccessDataService;

    @Autowired
    private IWorkLogService iWorkLogService;
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private AgilePluginService agilePluginService;

    @Autowired
    private RemoteIamOperator remoteIamOperator;

    private void setTo(Long issueId, BigDecimal predictionTime) {
        IssueConvertDTO issueConvertDTO = modelMapper.map(issueMapper.selectByPrimaryKey(issueId), IssueConvertDTO.class);
        issueConvertDTO.setRemainingTime(predictionTime);
        Long projectId = issueConvertDTO.getProjectId();
        List<String> fieldList = Arrays.asList(REMAINING_TIME_FIELD);
        this.self().updateRemainingTime(issueConvertDTO, projectId, issueId, fieldList);
    }

    private BigDecimal getRemainTime(IssueConvertDTO issueConvertDTO, BigDecimal theTime) {
        BigDecimal zero = new BigDecimal(0);
        return issueConvertDTO.getRemainingTime().subtract(theTime).compareTo(zero) < 0 ? zero : issueConvertDTO.getRemainingTime().subtract(theTime);
    }

    private void selfAdjustment(Long issueId, BigDecimal workTime) {
        IssueConvertDTO issueConvertDTO = modelMapper.map(issueMapper.selectByPrimaryKey(issueId), IssueConvertDTO.class);
        Long projectId = issueConvertDTO.getProjectId();
        List<String> fieldList = Arrays.asList(REMAINING_TIME_FIELD);
        if (issueConvertDTO.getRemainingTime() != null) {
            issueConvertDTO.setRemainingTime(getRemainTime(issueConvertDTO, workTime));
        } else {
            issueConvertDTO.setRemainingTime(BigDecimal.ZERO);
        }
        this.self().updateRemainingTime(issueConvertDTO, projectId, issueId, fieldList);
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldListName = "fieldList", instanceId = "issueId", idPosition = "arg")
    public void updateRemainingTime(IssueConvertDTO issueConvertDTO,
                                    Long projectId,
                                    Long issueId,
                                    List<String> fieldList) {
        issueAccessDataService.update(issueConvertDTO, new String[]{REMAINING_TIME_FIELD});
    }

    @Override
    public IssueWorkTimeCountVO countWorkTime(Long projectId, Long issueId) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (Objects.isNull(issueDTO)) {
            throw new CommonException("error.issue.not.exist");
        }
        return workLogMapper.countWorkTime(issueId, projectId);
    }

    @Override
    public WorkLogVO createWorkLog(Long projectId, WorkLogVO workLogVO) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(workLogVO.getIssueId());
        //登记工时前，校验工时的配置
        if (agilePluginService != null) {
            ProjectVO projectVO = remoteIamOperator.queryProject(projectId);
            if (projectVO == null) {
                throw new CommonException("error.project.empty");
            }
            agilePluginService.checkCreateWorkLog(projectVO.getOrganizationId(), projectVO.getId(), projectVO.getStatusId());
        }
        WorkLogValidator.checkCreateWorkLog(projectId, workLogVO, issueDTO);
        if (workLogVO.getResidualPrediction() != null) {
            switch (workLogVO.getResidualPrediction()) {
                case SELF_ADJUSTMENT:
                    selfAdjustment(workLogVO.getIssueId(), workLogVO.getWorkTime());
                    break;
                case NO_SET_PREDICTION_TIME:
                    break;
                case SET_TO:
                    setTo(workLogVO.getIssueId(), workLogVO.getPredictionTime());
                    break;
                case REDUCE:
                    selfAdjustment(workLogVO.getIssueId(), workLogVO.getPredictionTime());
                    break;
                default:
                    break;
            }
        }
        BaseFieldUtil.updateIssueLastUpdateInfo(workLogVO.getIssueId(), projectId);
        WorkLogDTO res = iWorkLogService.createBase(modelMapper.map(workLogVO, WorkLogDTO.class));
        return queryWorkLogById(res.getProjectId(), res.getLogId());
    }

    @Override
    public WorkLogVO updateWorkLog(Long projectId, Long logId, WorkLogVO workLogVO) {
        WorkLogValidator.checkUpdateWorkLog(workLogVO);
        workLogVO.setProjectId(projectId);
        WorkLogDTO res = updateBase(modelMapper.map(workLogVO, WorkLogDTO.class));
        BaseFieldUtil.updateIssueLastUpdateInfo(res.getIssueId(), res.getProjectId());
        return queryWorkLogById(res.getProjectId(), res.getLogId());
    }

    @Override
    public void deleteWorkLog(Long projectId, Long logId) {
        //删除工时前校验工时配置
        if (agilePluginService != null) {
            ProjectVO projectVO = remoteIamOperator.queryProject(projectId);
            if (projectVO == null) {
                throw new CommonException("error.project.empty");
            }
            agilePluginService.checkDeleteWorkLog(projectVO.getOrganizationId(), projectId, projectVO.getStatusId());
        }
        iWorkLogService.deleteBase(projectId, logId);
    }

    @Override
    public WorkLogVO queryWorkLogById(Long projectId, Long logId) {
        WorkLogDTO workLogDTO = new WorkLogDTO();
        workLogDTO.setProjectId(projectId);
        workLogDTO.setLogId(logId);
        WorkLogVO workLogVO = modelMapper.map(workLogMapper.selectOne(workLogDTO), WorkLogVO.class);
        workLogVO.setUserName(userService.queryUserNameByOption(workLogVO.getCreatedBy(), true).getRealName());
        return workLogVO;
    }

    @Override
    public List<WorkLogVO> queryWorkLogListByIssueId(Long projectId, Long issueId) {
        List<WorkLogVO> workLogVOList = modelMapper.map(workLogMapper.queryByIssueId(issueId, projectId), new TypeToken<List<WorkLogVO>>() {
        }.getType());
        List<Long> assigneeIds = workLogVOList.stream().filter(workLogVO -> workLogVO.getCreatedBy() != null && !Objects.equals(workLogVO.getCreatedBy(), 0L)).map(WorkLogVO::getCreatedBy).distinct().collect(Collectors.toList());
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
        workLogVOList.forEach(workLogVO -> {
            UserMessageDTO userMessageDTO = usersMap.get(workLogVO.getCreatedBy());
            if (userMessageDTO != null) {
                workLogVO.setUserName(userMessageDTO.getName());
                workLogVO.setUserImageUrl(userMessageDTO.getImageUrl());
                workLogVO.setRealName(userMessageDTO.getRealName());
                workLogVO.setLoginName(userMessageDTO.getLoginName());
            }
        });
        return workLogVOList;
    }

    @Override
    public WorkLogDTO updateBase(WorkLogDTO workLogDTO) {
        if (workLogMapper.updateByPrimaryKeySelective(workLogDTO) != 1) {
            throw new CommonException("error.workLog.update");
        }
        return workLogMapper.selectByPrimaryKey(workLogDTO.getLogId());
    }

}
