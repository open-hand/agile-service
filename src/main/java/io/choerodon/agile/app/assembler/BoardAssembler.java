package io.choerodon.agile.app.assembler;

import java.util.List;
import java.util.Map;

import io.choerodon.agile.api.vo.SearchVO;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

/**
 * @author jiaxu.cui@hand-china.com 2020/8/7 下午4:53
 */
@Component
public class BoardAssembler extends AbstractAssembler {

    public void handleOtherArgs(SearchVO searchVO) {
        Map<String, Object> args = searchVO.getOtherArgs();
        if (args != null) {
            assertNull(args, "sprint");
            assertNull(args, "version");
            assertNull(args, "fixVersion");
            assertNull(args, "influenceVersion");
            assertNull(args, "component");
            assertNull(args, "epic");
            assertNull(args, "label");
            assertNull(args, "assigneeId");
            assertNull(args, "feature");
            assertNull(args, "participantIds");
            assertNull(args, "mainResponsibleIds");
            assertNull(args, "productIds");
        }
    }

    public void handleAdvanceSearch(SearchVO searchVO) {
        Map<String, Object> args = searchVO.getAdvancedSearchArgs();
        if (args != null) {
            assertNull(args, "versionList");
            assertNull(args, "reporterIds");
        }
    }

    private void assertNull(Map<String, Object> args, String key) {
        List<String> list = (List<String>) args.get(key);
        String zero = "0";
        StringBuilder builder = new StringBuilder(key).append("Null");
        if (!ObjectUtils.isEmpty(list) && list.contains(zero)) {
            args.put(builder.toString(), true);
        }
    }
}
