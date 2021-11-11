package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-11-08 20:44
 */
public class WorkGroupUserRelVO {

    @Encrypt
    private List<Long> workGroupIds;

    @Encrypt
    private Long userId;

    private UserVO userVO;

    private List<WorkGroupVO> workGroupVOS;

    public List<Long> getWorkGroupIds() {
        return workGroupIds;
    }

    public void setWorkGroupIds(List<Long> workGroupIds) {
        this.workGroupIds = workGroupIds;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public UserVO getUserVO() {
        return userVO;
    }

    public void setUserVO(UserVO userVO) {
        this.userVO = userVO;
    }

    public List<WorkGroupVO> getWorkGroupVOS() {
        return workGroupVOS;
    }

    public void setWorkGroupVOS(List<WorkGroupVO> workGroupVOS) {
        this.workGroupVOS = workGroupVOS;
    }
}
