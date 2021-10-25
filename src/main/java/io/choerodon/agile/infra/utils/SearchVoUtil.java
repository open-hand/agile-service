package io.choerodon.agile.infra.utils;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.ObjectUtils;

import java.util.*;

/**
 * @author superlee
 * @since 2021-10-21
 */
public class SearchVoUtil {

    private static final String SPRINT = "sprint";
    private static final String TYPE_CODES = "typeCodes";

    public static boolean isSprintEmpty(SearchVO searchVO) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (!ObjectUtils.isEmpty(otherArgs)) {
            return ObjectUtils.isEmpty(otherArgs.get(SPRINT));
        }
        return true;
    }

    public static String getDimensionFromSearchVO(SearchVO searchVO) {
        Map<String, Object> searchArgs = searchVO.getSearchArgs();
        if (ObjectUtils.isEmpty(searchArgs)) {
            throw new CommonException("error.gantt.dimension.null");
        }
        return (String) searchArgs.get("dimension");
    }

    public static void setTypeCodes(SearchVO searchVO, List<String> typeCodes) {
        Map<String, Object> advancedSearchArgs = searchVO.getAdvancedSearchArgs();
        if (advancedSearchArgs == null) {
            advancedSearchArgs = new HashMap<>();
            searchVO.setAdvancedSearchArgs(advancedSearchArgs);
        }
        advancedSearchArgs.put(TYPE_CODES, typeCodes);
    }
}
