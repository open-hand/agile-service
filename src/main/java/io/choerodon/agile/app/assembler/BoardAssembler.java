package io.choerodon.agile.app.assembler;

import java.util.List;
import java.util.Map;

import io.choerodon.agile.api.vo.SearchVO;
import org.springframework.stereotype.Component;

/**
 * @author jiaxu.cui@hand-china.com 2020/8/7 下午4:53
 */
@Component
public class BoardAssembler extends AbstractAssembler {

    public void handleOtherArgs(SearchVO searchVO) {
        Map<String, Object> args = searchVO.getOtherArgs();
        if (args != null) {
            List<String> list = (List<String>) args.get("sprint");
            if (list != null && list.contains("0")) {
                args.put("sprintNull", true);
            }
            list = (List<String>) args.get("version");
            if (list != null && list.contains("0")) {
                args.put("versionNull", true);
            }
            list = (List<String>) args.get("component");
            if (list != null && list.contains("0")) {
                args.put("componentNull", true);
            }
            list = (List<String>) args.get("epic");
            if (list != null && list.contains("0")) {
                args.put("epicNull", true);
            }
            list = (List<String>) args.get("label");
            if (list != null && list.contains("0")) {
                args.put("labelNull", true);
            }
            list = (List<String>) args.get("assigneeId");
            if (list != null && list.contains("0")) {
                args.put("assigneeIdNull", true);
            }
        }
    }
}
