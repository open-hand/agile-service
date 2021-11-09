package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-11-08 15:31
 */
public class WorkGroupTreeVO {
   @Encrypt
   private List<Long> rootIds;

   List<WorkGroupVO> workGroupVOS;

    public List<Long> getRootIds() {
        return rootIds;
    }

    public void setRootIds(List<Long> rootIds) {
        this.rootIds = rootIds;
    }

    public List<WorkGroupVO> getWorkGroupVOS() {
        return workGroupVOS;
    }

    public void setWorkGroupVOS(List<WorkGroupVO> workGroupVOS) {
        this.workGroupVOS = workGroupVOS;
    }
}
