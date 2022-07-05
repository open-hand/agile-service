package io.choerodon.agile.infra.enums;

import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.infra.utils.SpringBeanUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * @author shinan.chen
 * @date 2018/10/23
 */
public enum InitStatus {
    PREPARE("准备", "prepare", StatusType.PREPARE),
    CREATE("待处理", "create", StatusType.TODO),
    PROCESSING("处理中", "processing", StatusType.DOING),
    COMPLETE("已完成", "complete", StatusType.DONE),
    ;
    String name;
    String code;
    String type;

    InitStatus(String name, String code, String type) {
        this.name = name;
        this.code = code;
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }

    public String getCode() {
        return code;
    }

    public static List<InitStatus> listByApplyType(String applyType) {
        List<InitStatus> result = new ArrayList<>();
        switch (applyType) {
            case SchemeApplyType.AGILE:
            case SchemeApplyType.WATERFALL:
            case SchemeApplyType.RISK:
                result.add(InitStatus.CREATE);
                result.add(InitStatus.PROCESSING);
                result.add(InitStatus.COMPLETE);
                break;
            case SchemeApplyType.PROGRAM:
                result.add(InitStatus.PREPARE);
                result.add(InitStatus.CREATE);
                result.add(InitStatus.PROCESSING);
                result.add(InitStatus.COMPLETE);
                break;
            default:
                break;
        }
        return result;
    }

    public static List<InitStatus> listInitStatus() {
        List<InitStatus> result = new ArrayList<>();
        AgilePluginService agilePluginService = SpringBeanUtil.getExpandBean(AgilePluginService.class);
        if (agilePluginService != null) {
            result.add(InitStatus.PREPARE);
            result.add(InitStatus.CREATE);
            result.add(InitStatus.PROCESSING);
            result.add(InitStatus.COMPLETE);
        } else {
            result.add(InitStatus.CREATE);
            result.add(InitStatus.PROCESSING);
            result.add(InitStatus.COMPLETE);
        }
        return result;
    }
}
