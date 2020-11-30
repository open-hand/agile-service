package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.SprintStoryPointVO;
import io.choerodon.agile.api.vo.SprintTaskVO;
import io.choerodon.agile.app.service.TeamPerformanceService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.mapper.TeamPerformanceMapper;
import io.choerodon.agile.infra.utils.DataUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Service
public class TeamPerformanceServiceImpl implements TeamPerformanceService {
    private static final String LEFT_PARENTHESIS = "(";
    private static final String RIGHT_PARENTHESIS = ")";

    @Autowired
    private TeamPerformanceMapper teamPerformanceMapper;

    @Autowired
    private UserService userService;

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
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(mainResponsibleIds, true);
        for (SprintTaskVO sprintTask : sprintTasks) {
            handleUser(sprintTask, usersMap.get(sprintTask.getMainResponsibleId()));
            handleMainRate(sumRemainingTime, remainingTimeComplete, sprintTask);
        }
        return sprintTasks;
    }

    private void handleMainRate(BigDecimal sumStoryPoints, BigDecimal sumStoryPointsComplete, SprintTaskVO sprintTask) {
        sprintTask.setRemainingTimeRate(DataUtil.divide(
                DataUtil.multiply(sprintTask.getRemainingTime(), new BigDecimal(100)),
                sumStoryPoints, 2));
        sprintTask.setRemainingTimeCompleteRate(DataUtil.divide(
                DataUtil.multiply(sprintTask.getRemainingTimeComplete(), new BigDecimal(100)),
                sumStoryPointsComplete, 2));
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