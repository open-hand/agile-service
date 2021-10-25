package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserMessageDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;
import java.util.Date;
import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2021-10-20 16:44
 */
public class WorkHoursCalendarVO {

    @Encrypt
    private Long userId;

    private BigDecimal allEstimateTime;

    private UserMessageDTO userMessageDTO;

    private Map<String, BigDecimal> countMap;

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public UserMessageDTO getUserMessageDTO() {
        return userMessageDTO;
    }

    public void setUserMessageDTO(UserMessageDTO userMessageDTO) {
        this.userMessageDTO = userMessageDTO;
    }

    public Map<String, BigDecimal> getCountMap() {
        return countMap;
    }

    public void setCountMap(Map<String, BigDecimal> countMap) {
        this.countMap = countMap;
    }

    public BigDecimal getAllEstimateTime() {
        return allEstimateTime;
    }

    public void setAllEstimateTime(BigDecimal allEstimateTime) {
        this.allEstimateTime = allEstimateTime;
    }
}
