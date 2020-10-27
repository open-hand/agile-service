package io.choerodon.agile.app.listener;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.NoticeEventVO;
import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.app.service.StatusNoticeSettingService;
import io.choerodon.agile.infra.dto.ConfigurationRuleReceiverDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.RuleNoticeEvent;
import io.choerodon.agile.infra.mapper.ConfigurationRuleMapper;
import io.choerodon.agile.infra.mapper.ConfigurationRuleReceiverMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.RuleEventUtil;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.domain.AuditDomain;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.hzero.boot.message.MessageClient;
import org.hzero.boot.message.entity.MessageSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * @author jiaxu.cui@hand-china.com 2020/10/9 上午9:59
 */
@Component
public class IssueNoticeListener {

    public static final String ISSUE = "ISSUE";

    public static final Logger log = LoggerFactory.getLogger(IssueNoticeListener.class);

    @Autowired
    private ConfigurationRuleMapper configurationRuleMapper;
    @Autowired
    private MessageClient messageClient;
    @Autowired
    private ConfigurationRuleService configurationRuleService;
    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private ConfigurationRuleReceiverMapper configurationRuleReceiverMapper;

    @Async
    @TransactionalEventListener(NoticeEventVO.class)
    public void issueNoticeDetection(NoticeEventVO noticeEvent){
        if (RuleEventUtil.checkEvent(noticeEvent, ISSUE)){
            return;
        }
        Long projectId = noticeEvent.getProjectId();
        Set<String> fieldList = noticeEvent.getFieldList();
        String event = noticeEvent.getEvent();
        boolean allFieldCheck = BooleanUtils.isTrue(noticeEvent.getAllFieldCheck());
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(noticeEvent.getInstanceId());
        ConfigurationRuleVO rule = new ConfigurationRuleVO();
        rule.setProjectId(projectId);
        rule.setTypeCode(issueDTO.getTypeCode());
        // 筛选出检测更新字段的规则
        List<ConfigurationRuleVO> ruleVOList = configurationRuleService.processRule(configurationRuleMapper.selectByProjectId(rule), fieldList, allFieldCheck);
        // 检查issue是否符合页面规则条件
        Map<String, Long> map = CollectionUtils.isEmpty(ruleVOList) ? new HashMap<>() :
                configurationRuleMapper.selectByRuleList(issueDTO.getIssueId(), projectId, ruleVOList);
        // 获取所有符合的ruleId
        List<Long> ruleIdList = Optional.ofNullable(map).orElse(new HashMap<>())
                .values().stream().filter(Objects::nonNull).collect(Collectors.toList());
        // 组装符合条件的页面规则messageSender
        List<MessageSender> ruleSenderList = generateRuleSender(event, projectId, ruleIdList, issueDTO, fieldList);
        // 合并消息通知并发送
        for (MessageSender messageSender : ruleSenderList) {
            messageClient.async().sendMessage(messageSender);
        }
        // 更改页面规则对应的处理人
        changeProcesser(issueDTO, ruleIdList, ruleVOList);
    }

    /**
     * 修改经办人
     * @param issueDTO issueDTO
     * @param ruleIdList 符合条件的规则id
     * @param ruleVOList 全部的规则
     */
    private void changeProcesser(IssueDTO issueDTO, List<Long> ruleIdList, List<ConfigurationRuleVO> ruleVOList) {
        if (CollectionUtils.isEmpty(ruleIdList)){
            return;
        }
        ConfigurationRuleVO rule = ruleVOList.stream()
                .filter(ruleVO -> ruleIdList.contains(ruleVO.getId()))
                .max(Comparator.comparing(AuditDomain::getCreationDate))
                .orElse(new ConfigurationRuleVO());
        List<ConfigurationRuleReceiverDTO> receiverList =
                configurationRuleReceiverMapper.selectReceiver(Collections.singletonList(rule.getId()),
                Collections.singletonList(ConfigurationRuleReceiverDTO.TYPE_PROCESSER));
        ConfigurationRuleReceiverDTO receiver = receiverList.stream().findFirst().orElse(null);
        if (Objects.isNull(receiver)){
            return;
        }
        issueDTO.setAssigneeId(receiver.getId());
        issueMapper.updateOptional(issueDTO, "assigneeId");
    }

    private List<MessageSender> generateRuleSender(String event,Long projectId,List<Long> ruleIdList,
                                                   IssueDTO issue, Set<String> fieldList) {
        Map<Long, ConfigurationRuleVO> map = configurationRuleService.selectRuleALLReceiver(ruleIdList);
        // 生成需要合并的messageSenderList
        return Stream.of(Arrays.stream(RuleNoticeEvent.getMsgCode(event))
                    .map(code -> generatedSenderByCode(code, projectId, issue, fieldList).apply(sendMsgUtil))
                    .filter(Objects::nonNull).collect(Collectors.toList()),
                    sendMsgUtil.generateAutoRuleTriggerSender(DetailsHelper.getUserDetails().getUserId(), issue,  map.values(), event))
                .flatMap(Collection::stream).collect(Collectors.toList());
    }

    private Function<SendMsgUtil, MessageSender> generatedSenderByCode(String messageCode, Long projectId,
                                                                       IssueDTO issue, Set<String> fieldList){
        Function<SendMsgUtil, MessageSender> func = null;
        switch (messageCode){
            case RuleNoticeEvent.ISSUECREATE:
                func = msgUtil -> msgUtil.generateIssueCreatesender(projectId, issue);
                break;
            case RuleNoticeEvent.ISSUEASSIGNEE:
                func = msgUtil -> msgUtil.generateIssueAsigneeSender(projectId, fieldList, issue);
                break;
            case RuleNoticeEvent.ISSUESOLVE:
                func = msgUtil -> msgUtil.generateIssueResolvSender(projectId, fieldList, issue);
                break;
            case RuleNoticeEvent.ISSUECHANGESTATUS:
                StatusNoticeSettingVO settingVO = statusNoticeSettingService.selectNoticeUserAndType(projectId, issue.getIssueId());
                func = msgUtil -> msgUtil.generateNoticeIssueStatusSender(projectId, settingVO.getUserIdList(),
                        new ArrayList<>(settingVO.getUserTypeList()), issue, DetailsHelper.getUserDetails(), fieldList);
                break;
            default:
                break;
        }
        return func;
    }
}
