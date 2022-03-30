package io.choerodon.agile.infra.support;

/**
 * @author scp
 * 同步待办到第三方
 * @since 2022/3/29
 */
public interface OpenAppIssueSyncConstant {
    // 操作类型
    enum OperationType {
        CREATE, UPDATE, DELETE
    }
    // 同步状态
    enum Status {
        PROCESSING, SUCCESS, FAILED
    }
    // 第三方类型
    enum AppType{
        DIND("ding_talk"); //钉钉
        private String value;

        AppType(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

}
