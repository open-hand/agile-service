package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.StarBeaconVO;
/**
 * 服务接口
 *
 * @author jiaxu.cui@hand-china.com
 */
public interface StarBeaconService {


    void starInstance(StarBeaconVO starBeaconVO);

    void unStarInstance(StarBeaconVO starBeaconVO);

}

