package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.SprintBugVO;
import io.choerodon.agile.api.vo.SprintStoryPointVO;
import io.choerodon.agile.api.vo.SprintTaskVO;
import io.choerodon.agile.app.service.TeamPerformanceService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.TeamPerformanceMapper;
import io.choerodon.agile.infra.utils.DataUtil;
import io.choerodon.agile.infra.utils.ListUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class TeamPerformanceServiceImpl implements TeamPerformanceService {
    private static final String LEFT_PARENTHESIS = "(";
    private static final String RIGHT_PARENTHESIS = ")";

    @Autowired
    private TeamPerformanceMapper teamPerformanceMapper;

    @Autowired
    private UserService userService;

    @Autowired
    private BaseFeignClient baseFeignClient;

    @Override
    public List<SprintStoryPointVO> querySprintStoryPoint(Long projectId) {
        List<SprintStoryPointVO> sprintStoryPoints = teamPerformanceMapper.querySprintStoryPoints(projectId);
        BigDecimal sumStoryPoints = new BigDecimal(0);
        BigDecimal sumStoryPointsComplete = new BigDecimal(0);
        List<Long> mainResponsibleIds = new ArrayList<>();
        for (SprintStoryPointVO sprintStoryPoint : sprintStoryPoints) {
            if (Objects.nonNull(sprintStoryPoint)) {
                sumStoryPoints = DataUtil.add(sumStoryPoints, sprintStoryPoint.getStoryPoints());
                sumStoryPointsComplete = DataUtil.add(sumStoryPointsComplete, sprintStoryPoint.getStoryPointsComplete());
                Long mainResponsibleId = sprintStoryPoint.getMainResponsibleId();
                if (Objects.nonNull(mainResponsibleId)) {
                    mainResponsibleIds.add(mainResponsibleId);
                }
            }
        }
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(mainResponsibleIds, true);
        for (SprintStoryPointVO sprintStoryPoint : sprintStoryPoints) {
            handleUser(sprintStoryPoint, usersMap);
            handleMainRate(sumStoryPoints, sumStoryPointsComplete, sprintStoryPoint);
        }
        return sprintStoryPoints;
    }

    @Override
    public List<SprintTaskVO> querySprintTaskTime(Long projectId) {
        List<SprintTaskVO> sprintTasks = teamPerformanceMapper.querySprintTaskTime(projectId);
        BigDecimal sumRemainingTime = new BigDecimal(0);
        BigDecimal remainingTimeComplete = new BigDecimal(0);
        List<Long> mainResponsibleIds = new ArrayList<>();
        for (SprintTaskVO sprintTask : sprintTasks) {
            if (Objects.nonNull(sprintTask)) {
                sumRemainingTime = DataUtil.add(sumRemainingTime, sprintTask.getRemainingTime());
                remainingTimeComplete = DataUtil.add(remainingTimeComplete, sprintTask.getRemainingTimeComplete());
                Long mainResponsibleId = sprintTask.getMainResponsibleId();
                if (Objects.nonNull(mainResponsibleId)) {
                    mainResponsibleIds.add(mainResponsibleId);
                }
            }
        }
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(ListUtil.filterByKey(mainResponsibleIds, 0L), true);
        for (SprintTaskVO sprintTask : sprintTasks) {
            handleUser(sprintTask, usersMap.get(sprintTask.getMainResponsibleId()));
            handleMainRate(sumRemainingTime, remainingTimeComplete, sprintTask);
        }
        return sprintTasks;
    }

    @Override
    public Page<SprintBugVO> querySprintBugRank(Long projectId, String environment, String type, PageRequest pageRequest) {
        Page<SprintBugVO> sprintBugPage = PageHelper.doPageAndSort(pageRequest,
                () -> teamPerformanceMapper.querySprintBugCount(projectId,environment, type));
        sprintBugPage.setContent(handleUser(sprintBugPage.getContent()));
        return sprintBugPage;
    }

    @Override
    public List<SprintBugVO> querySprintBugCount(Long projectId, String environment, String type) {
        List<SprintBugVO> sprintBugs = teamPerformanceMapper.querySprintBugCount(projectId, environment, type);
        return handleUser(sprintBugs);
    }

    @Override
    public List<SprintStoryPointVO> queryHistorySprintStoryPoint(Long projectId) {
        return teamPerformanceMapper.queryHistorySprintStoryPoint(projectId);
    }

    @Override
    public List<SprintTaskVO> queryHistorySprintTaskTime(Long projectId) {
        return teamPerformanceMapper.queryHistorySprintTaskTime(projectId);
    }

    @Override
    public List<SprintBugVO> queryHistorySprintBugCount(Long projectId, String environment, String type, Boolean other, List<Long> responsibleIds) {
        List<SprintBugVO> sprintBugs = teamPerformanceMapper.queryHistorySprintBugCount(projectId, environment,
                type, other, responsibleIds);
        return handleUser(sprintBugs);
    }

    @Override
    public List<UserDTO> queryResponsible(Long projectId) {
        List<Long> responsibleIds = teamPerformanceMapper.queryResponsible(projectId);
        return obtainUser(responsibleIds);
    }

    private List<UserDTO> obtainUser(List<Long> responsibleIds) {
        List<Long> realResponsibleIds =
                responsibleIds.stream().filter(responsibleId -> Objects.nonNull(responsibleId)).collect(Collectors.toList());
        Long[] assigneeIds = new Long[realResponsibleIds.size()];
        realResponsibleIds.toArray(assigneeIds);
        List<UserDTO> users = baseFeignClient.listUsersByIds(assigneeIds, false).getBody();
        if(responsibleIds.size() != realResponsibleIds.size()){
            users.add(new UserDTO());
        }
        return users;
    }

    private List<SprintBugVO> handleUser(List<SprintBugVO> sprintBugs) {
        if (CollectionUtils.isEmpty(sprintBugs)) {
            return sprintBugs;
        }
        List<Long> mainResponsibleIds =
                sprintBugs.stream().filter(sprintBugVO -> Objects.nonNull(sprintBugVO.getResponsibleId()))
                        .map(SprintBugVO::getResponsibleId).collect(Collectors.toList());
        mainResponsibleIds = ListUtil.filterByKey(mainResponsibleIds, 0L);
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(mainResponsibleIds, true);
        sprintBugs.forEach(sprintBug -> handleUser(sprintBug, usersMap.get(sprintBug.getResponsibleId())));
        return sprintBugs;
    }

    private void handleUser(SprintBugVO sprintBug, UserMessageDTO userMessage) {
        if (Objects.isNull(userMessage)) {
            return;
        }
        sprintBug.setRealName(userMessage.getRealName());
        sprintBug.setLoginName(userMessage.getLoginName());
        sprintBug.setImageUrl(userMessage.getImageUrl());
        sprintBug.setName(new StringBuilder()
                .append(userMessage.getRealName())
                .append(LEFT_PARENTHESIS)
                .append(userMessage.getLoginName())
                .append(RIGHT_PARENTHESIS).toString());
    }

    private void handleMainRate(BigDecimal sumStoryPoints, BigDecimal sumStoryPointsComplete, SprintTaskVO sprintTask) {
        sprintTask.setRemainingTimeRate(DataUtil.divide(
                DataUtil.multiply(sprintTask.getRemainingTime(), new BigDecimal(100)),
                sumStoryPoints, 2));

        sprintTask.setRemainingTimeCompleteRate(DataUtil.divide(
                DataUtil.multiply(sprintTask.getRemainingTimeComplete(), new BigDecimal(100)),
                sumStoryPointsComplete, 2));


        BigDecimal remainingTimePlanCompleteRate = Objects.isNull(sprintTask.getRemainingTime()) ||
                sprintTask.getRemainingTime().compareTo(new BigDecimal(0)) == 0 ?
                new BigDecimal(0) : DataUtil.divide(
                DataUtil.multiply(sprintTask.getRemainingTimeComplete(), new BigDecimal(100)),
                sprintTask.getRemainingTime(), 2);
        sprintTask.setRemainingTimePlanCompleteRate(remainingTimePlanCompleteRate);
    }

    private void handleUser(SprintTaskVO sprintTask, UserMessageDTO mainResponsible) {
        if (Objects.isNull(mainResponsible)) {
            return;
        }
        sprintTask.setLoginName(mainResponsible.getLoginName());
        sprintTask.setRealName(mainResponsible.getRealName());
        sprintTask.setName(new StringBuilder()
                .append(mainResponsible.getRealName())
                .append(LEFT_PARENTHESIS)
                .append(mainResponsible.getLoginName())
                .append(RIGHT_PARENTHESIS).toString());
    }

    private void handleMainRate(BigDecimal sumStoryPoints, BigDecimal sumStoryPointsComplete, SprintStoryPointVO sprintStoryPoint) {
        sprintStoryPoint.setMainStoryPointsRate(DataUtil.divide(
                DataUtil.multiply(sprintStoryPoint.getStoryPoints(), new BigDecimal(100)),
                sumStoryPoints, 2));

        sprintStoryPoint.setMainStoryPointsCompleteRate(DataUtil.divide(
                DataUtil.multiply(sprintStoryPoint.getStoryPointsComplete(), new BigDecimal(100)),
                sumStoryPointsComplete, 2));

        BigDecimal mainStoryPointsPlanCompleteRate = Objects.isNull(sprintStoryPoint.getStoryPoints()) ||
                sprintStoryPoint.getStoryPoints().compareTo(new BigDecimal(0)) == 0 ?
                new BigDecimal(0) : DataUtil.divide(
                DataUtil.multiply(sprintStoryPoint.getStoryPointsComplete(), new BigDecimal(100)),
                sprintStoryPoint.getStoryPoints(), 2);
        sprintStoryPoint.setMainStoryPointsPlanCompleteRate(mainStoryPointsPlanCompleteRate);

    }


    private void handleUser(SprintStoryPointVO sprintStoryPointVO, Map<Long, UserMessageDTO> usersMap) {
        UserMessageDTO mainResponsible = usersMap.get(sprintStoryPointVO.getMainResponsibleId());
        if (Objects.isNull(mainResponsible)) {
            return;
        }
        sprintStoryPointVO.setLoginName(mainResponsible.getLoginName());
        sprintStoryPointVO.setRealName(mainResponsible.getRealName());
        sprintStoryPointVO.setName(new StringBuilder()
                .append(mainResponsible.getRealName())
                .append(LEFT_PARENTHESIS)
                .append(mainResponsible.getLoginName())
                .append(RIGHT_PARENTHESIS).toString());
    }
}