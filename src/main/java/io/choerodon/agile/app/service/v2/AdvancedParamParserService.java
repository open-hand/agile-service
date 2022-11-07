package io.choerodon.agile.app.service.v2;

import java.util.Set;

import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.infra.enums.InstanceType;

/**
 * @author superlee
 * @since 2022-11-02
 */
public interface AdvancedParamParserService {

    String parse(InstanceType instanceType,
                 SearchParamVO searchParamVO,
                 Set<Long> projectIds);

}
