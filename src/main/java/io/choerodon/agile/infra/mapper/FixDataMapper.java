package io.choerodon.agile.infra.mapper;

import org.apache.ibatis.annotations.Param;

/**
 * @author zhaotianxin
 * @date 2022/1/12 14:40
 */
public interface FixDataMapper {


    void initBoardType(@Param("type") String type);

    void initBoardTemplateType(@Param("type") String type);
}
