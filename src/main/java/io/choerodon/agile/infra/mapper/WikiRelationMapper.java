package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.WikiRelationDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/12/03.
 * Email: fuqianghuang01@gmail.com
 */
public interface WikiRelationMapper extends BaseMapper<WikiRelationDTO> {

    void updateByOptions(@Param("id") Long id, @Param("spaceId") Long spaceId);
}
