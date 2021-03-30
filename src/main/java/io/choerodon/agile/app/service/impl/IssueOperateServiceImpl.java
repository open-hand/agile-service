package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import io.choerodon.agile.api.vo.BatchUpdateFieldStatusVO;
import io.choerodon.agile.app.service.IssueOperateService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-11-27 17:06
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueOperateServiceImpl implements IssueOperateService {
    private static final String WEBSOCKET_BATCH_DELETE_ISSUE = "agile-batch-delete-issue";

    @Autowired
    private IssueService issueService;
    @Autowired
    private MessageClientC7n messageClientC7n;
    @Autowired
    private UserService userService;
    @Autowired
    private IssueMapper issueMapper;

    @Async
    @Override
    public void batchDeleteIssue(Long projectId, List<Long> issueIds) {
        if (!CollectionUtils.isEmpty(issueIds)) {
            String messageCode = WEBSOCKET_BATCH_DELETE_ISSUE + "-" + projectId;
            Long userId = DetailsHelper.getUserDetails().getUserId();
            BatchUpdateFieldStatusVO batchUpdateFieldStatusVO = new BatchUpdateFieldStatusVO();
            batchUpdateFieldStatusVO.setKey(messageCode);
            batchUpdateFieldStatusVO.setUserId(userId);
            boolean projectOwner = userService.isProjectOwner(projectId, userId);
            if (Boolean.FALSE.equals(projectOwner)) {
                batchUpdateFieldStatusVO.setStatus("failed");
                batchUpdateFieldStatusVO.setError("您无删除权限");
                messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
                return;
            }
            Double progress = 0.0;
            double incremental = Math.ceil(issueIds.size() <= 20 ? 1 : (issueIds.size()*1.0) / 20) ;
            try {
                batchUpdateFieldStatusVO.setStatus("doing");
                batchUpdateFieldStatusVO.setProcess(progress);
                messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
                // 查询子任务ids
                List<Long> subList = new ArrayList<>();
                subList.addAll(issueMapper.selectSubListByIssueIds(projectId,issueIds));
                int i = 0;
                for (Long issueId : issueIds) {
                    i++;
                    // 删除任务会附带删除子任务,因此需要判断问题是否被删除ss
                    if (!subList.contains(issueId)) {
                        issueService.deleteIssue(projectId, issueId);
                    }
                    if (i % incremental == 0) {
                        batchUpdateFieldStatusVO.setProcess((i * 1.0) / issueIds.size());
                    }
                    messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
                }
                batchUpdateFieldStatusVO.setStatus("success");
                batchUpdateFieldStatusVO.setProcess(1.0);
            } catch (Exception e) {
                batchUpdateFieldStatusVO.setStatus("failed");
                batchUpdateFieldStatusVO.setError(e.getMessage());
                throw new CommonException("delete issue failed, exception: {}", e.getMessage());
            } finally {
                messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
            }
        }
    }
}
