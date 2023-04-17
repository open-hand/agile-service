package io.choerodon.agile.app.service.impl;

import java.util.*;

import com.alibaba.fastjson.JSON;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

import io.choerodon.agile.api.vo.BatchUpdateFieldStatusVO;
import io.choerodon.agile.api.vo.CopyConditionVO;
import io.choerodon.agile.api.vo.LinkIssueLinkageMessageVO;
import io.choerodon.agile.app.service.AgileWaterfallService;
import io.choerodon.agile.app.service.IssueOperateService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.core.base.BaseConstants;
import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.hzero.websocket.helper.SocketSendHelper;

/**
 * @author zhaotianxin
 * @date 2020-11-27 17:06
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueOperateServiceImpl implements IssueOperateService {
    private static final String WEBSOCKET_BATCH_DELETE_ISSUE = "agile-batch-delete-issue";
    private static final String WEBSOCKET_EXECUTION_LINK_ISSUE_LINKAGE = "agile-execution-link-issue-linkage";

    @Autowired
    private IssueService issueService;
    @Autowired
    private SocketSendHelper socketSendHelper;
    @Autowired
    private UserService userService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;

    @Async
    @Override
    public void batchDeleteIssue(Long projectId, List<Long> issueIds) {
        if (CollectionUtils.isEmpty(issueIds)) {
            return;
        }
        String messageCode = WEBSOCKET_BATCH_DELETE_ISSUE + BaseConstants.Symbol.MIDDLE_LINE + projectId;
        final CustomUserDetails userDetails = DetailsHelper.getUserDetails();
        Assert.notNull(userDetails, BaseConstants.ErrorCode.NOT_LOGIN);
        BatchUpdateFieldStatusVO batchUpdateFieldStatusVO = new BatchUpdateFieldStatusVO();
        final Long userId = userDetails.getUserId();
        batchUpdateFieldStatusVO.setKey(messageCode);
        batchUpdateFieldStatusVO.setUserId(userId);
        boolean projectOwner = userService.isProjectOwner(projectId, userId);
        if (!Boolean.TRUE.equals(projectOwner) && !Boolean.TRUE.equals(userDetails.getAdmin())) {
            batchUpdateFieldStatusVO.setStatus("failed");
            batchUpdateFieldStatusVO.setError("您无删除权限");
            socketSendHelper.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
            return;
        }
        Double progress = 0.0;
        double lastSendProcess = 0D;
        try {
            batchUpdateFieldStatusVO.setStatus("doing");
            batchUpdateFieldStatusVO.setProcess(progress);
            socketSendHelper.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
            // 查询子任务ids
            List<Long> subList = new ArrayList<>(issueMapper.selectSubListByIssueIds(projectId, issueIds));
            int i = 0;
            for (Long issueId : issueIds) {
                i++;
                // 删除任务会附带删除子任务,因此需要判断问题是否被删除ss
                if (!subList.contains(issueId)) {
                    issueService.deleteIssueOnRequiresNew(projectId, issueId, batchUpdateFieldStatusVO);
                }
                double process = (i * 1.0) / issueIds.size();
                if (process - lastSendProcess >= 0.1) {
                    batchUpdateFieldStatusVO.setProcess(process);
                    socketSendHelper.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
                    lastSendProcess = process;
                }
            }
            batchUpdateFieldStatusVO.setStatus("success");
            batchUpdateFieldStatusVO.setProcess(1.0);
        } catch (Exception e) {
            batchUpdateFieldStatusVO.setStatus("failed");
            batchUpdateFieldStatusVO.setError(e.getMessage());
            throw new CommonException("delete issue failed, exception: {}", e.getMessage());
        } finally {
            socketSendHelper.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
        }
    }

    @Async
    @Override
    public void updateIssueStatusLinkage(Long projectId, Long issueId, IssueDTO issueDTO, String applyType, String encryptType, RequestAttributes requestAttributes) {
        EncryptContext.setEncryptType(encryptType);
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Boolean isSub = Objects.equals("sub_task",issueDTO.getTypeCode()) || (Objects.equals("bug",issueDTO.getTypeCode()) && !ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && !Objects.equals(issueDTO.getRelateIssueId(), 0L));
        if (Boolean.TRUE.equals(isSub)) {
            return;
        }
        Set<Long> influenceIssueIds = new HashSet<>();
        // 获取当前的issue
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String websocketKey = WEBSOCKET_EXECUTION_LINK_ISSUE_LINKAGE + BaseConstants.Symbol.MIDDLE_LINE + projectId;
        LinkIssueLinkageMessageVO linkIssueLinkageMessageVO = new LinkIssueLinkageMessageVO();
        linkIssueLinkageMessageVO.setKey(websocketKey);
        try {
            if (agileWaterfallService != null && Arrays.asList(IssueTypeCode.WATERFALL_ISSUE_TYPE_CODE).contains(issueDTO.getTypeCode())) {
                agileWaterfallService.updatePredecessorIssueStatus(projectId, issueId, issueDTO, applyType, influenceIssueIds);
            } else {
                issueService.updateLinkIssueStatus(projectId, issueId, issueDTO, applyType, influenceIssueIds);
            }
            String statusCode = CollectionUtils.isEmpty(influenceIssueIds) ? "success" : "failed";
            linkIssueLinkageMessageVO.setStatusCode(statusCode);
        } catch (Exception e) {
            linkIssueLinkageMessageVO.setStatusCode("failed");
            linkIssueLinkageMessageVO.setMessage("error.link.issue.linkage.execution");
            throw new CommonException("error.link.issue.linkage.execution", e);
        } finally {
            socketSendHelper.sendByUserId(userId, websocketKey, JSON.toJSONString(linkIssueLinkageMessageVO));
        }
    }

    @Async
    @Override
    public void asyncCloneIssueByIssueId(Long projectId,
                                         Long issueId,
                                         CopyConditionVO copyConditionVO,
                                         Long organizationId,
                                         String applyType,
                                         String asyncTraceId) {
        issueService.cloneIssueByIssueId(projectId, issueId, copyConditionVO, organizationId, applyType, asyncTraceId);
    }
}
