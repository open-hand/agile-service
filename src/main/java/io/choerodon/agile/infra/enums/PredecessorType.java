package io.choerodon.agile.infra.enums;

/**
 * @author superlee
 * @since 2021-11-10
 */
public enum PredecessorType {
    /**
     * 结束到结束
     */
    FF,
    /**
     * 结束到开始
     */
    FS,
    /**
     * 开始到开始
     */
    SS,
    /**
     * 开始到结束
     */
    SF;

    public static boolean contains(String value) {
        for (PredecessorType predecessorType : PredecessorType.values()) {
            if (predecessorType.name().equalsIgnoreCase(value)) {
                return true;
            }
        }
        return false;
    }
}
